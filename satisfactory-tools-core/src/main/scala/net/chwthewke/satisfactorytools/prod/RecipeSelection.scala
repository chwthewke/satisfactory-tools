package net.chwthewke.satisfactorytools
package prod

import cats.data.NonEmptyList
import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.option._
import cats.syntax.reducible._
import cats.syntax.show._
import cats.syntax.traverse._

import data.ProductionConfig
import model.Countable
import model.ExtractionRecipe
import model.Form
import model.Item
import model.Machine
import model.Model
import model.Options
import model.Recipe
import model.ResourceDistrib
import model.ResourcePurity

case class RecipeSelection(
    allowedRecipes: Vector[Recipe[Machine, Item]],
    extractionRecipes: Map[Item, Vector[( ResourceDistrib, Recipe[Machine, Item] )]],
    tieredExtractionRecipes: Map[Item, Vector[ExtractionRecipe]],
    resourceCaps: Map[Item, Double],
    resourceWeights: Map[Item, Double] // TODO option to make it quadratic
)

object RecipeSelection {
  def extractionRecipes(
      model: Model,
      options: Options
  ): Map[Item, Vector[( ResourceDistrib, Recipe[Machine, Item] )]] =
    model.extractionRecipes
      .foldMap {
        case ( product, recipe ) =>
          (
            options.scoreExtractionRecipe( recipe ),
            model.resourceNodes.get( recipe.producedIn ).flatMap( _.get( product ) )
          ).mapN( ( score, dist ) => Map( product -> Vector( ( score, ( dist, recipe ) ) ) ) ).orEmpty
      }
      .map {
        case ( item, recipes ) => ( item, recipes.sortBy( _._1 ).map( _._2 ) )
      }

  def tieredExtractionRecipes(
      extractionRecipes: Map[Item, Vector[( ResourceDistrib, Recipe[Machine, Item] )]],
      options: Options
  ): Map[Item, Vector[ExtractionRecipe]] =
    extractionRecipes.map {
      case ( item, distRecipes ) =>
        ( item, distRecipes.foldMap { case ( dist, recipe ) => tieredExtractionRecipes( dist, recipe, options ) } )
    }

  def tieredExtractionRecipes(
      distrib: ResourceDistrib,
      recipe: Recipe[Machine, Item],
      options: Options
  ): Vector[ExtractionRecipe] =
    Vector(
      ( distrib.pureNodes, ResourcePurity.Pure ),
      ( distrib.normalNodes, ResourcePurity.Normal ),
      ( distrib.impureNodes, ResourcePurity.Impure )
    ).flatMap {
      case ( nodeCount, purity ) =>
        Option.when( nodeCount > 0 )(
          configuredRecipe( recipe, options, purity ) match {
            case ( recipe, clockSpeed ) => ExtractionRecipe( recipe, clockSpeed, nodeCount )
          }
        )
    }

  def configuredRecipe(
      source: Recipe[Machine, Item],
      options: Options,
      purity: ResourcePurity
  ): ( Recipe[Machine, Item], Double ) = {
    val configuredProduct: NonEmptyList[( Countable[Item, Double], Double )] = source.product.map {
      case Countable( item, amount ) =>
        val maxAmountPerMinute: Int =
          if (item.form == Form.Solid) options.belt.itemsPerMinute else options.pipe.cubicMetersPerMinute

        val maxOutput: Double =
          maxAmountPerMinute.toDouble / 60000d * source.duration.toMillis

        val clockReq            = options.clockSpeed.percent.toDouble
        val clockOutput: Double = amount * purity.value * clockReq / 100d

        val ( clock, output ) =
          if (maxOutput > clockOutput)
            ( clockReq, clockOutput )
          else
            ( 100d * maxOutput / (amount * purity.value), maxOutput )

        ( Countable( item, output ), clock )
    }

    val finalClock = configuredProduct.map( _._2 ).maximum

    (
      source.copy(
        displayName = show"${source.displayName} (${purity.entryName}, ${finalClock.round}%)",
        product = configuredProduct.map( _._1 )
      ),
      finalClock
    )
  }

  def resourceCaps( tieredExtractionRecipes: Map[Item, Vector[ExtractionRecipe]] ): Map[Item, Double] =
    tieredExtractionRecipes.map {
      case ( item, tiers ) =>
        ( item, tiers.foldMap {
          case ExtractionRecipe( recipe, _, count ) =>
            recipe.productsPerMinute.find( _.item == item ).fold( 0d )( _.amount * count )
        } )
    }

  def init( model: Model, config: ProductionConfig, options: Options ): Either[String, RecipeSelection] =
    config.recipes
      .traverse( cn => model.manufacturingRecipes.find( _.className == cn ).toValidNel( cn.show ) )
      .leftMap( missing => show"Unknown recipe(s) in config: ${missing.mkString_( ", " )}" )
      .toEither
      .map {
        val extraction = extractionRecipes( model, options )
        val tiered     = tieredExtractionRecipes( extraction, options )
        val caps       = resourceCaps( tiered )

        RecipeSelection(
          _,
          extractionRecipes( model, options ),
          tiered,
          caps,
          caps.map { case ( item, cap ) => ( item, 1e4d / cap ) }
        )
      }
}
