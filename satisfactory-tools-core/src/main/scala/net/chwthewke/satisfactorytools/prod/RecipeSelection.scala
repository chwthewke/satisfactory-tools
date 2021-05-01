package net.chwthewke.satisfactorytools
package prod

import cats.data.NonEmptyList
import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.reducible._
import cats.syntax.show._

import model.Countable
import model.ExtractionRecipe
import model.Form
import model.Item
import model.Machine
import model.MapOptions
import model.Model
import model.Options
import model.Recipe
import model.RecipeList
import model.ResourceDistrib
import model.ResourcePurity

case class RecipeSelection(
    allowedRecipes: Vector[( Recipe[Machine, Item], Double )],
    extractionRecipes: Map[Item, Vector[( ResourceDistrib, Recipe[Machine, Item] )]],
    tieredExtractionRecipes: Map[Item, Vector[ExtractionRecipe]],
    resourceCaps: Map[Item, Double],
    resourceWeights: Map[Item, Double]
)

object RecipeSelection {
  def extractionRecipes(
      model: Model,
      options: Options,
      map: MapOptions
  ): Map[Item, Vector[( ResourceDistrib, Recipe[Machine, Item] )]] =
    model.extractionRecipes
      .foldMap {
        case ( product, recipe ) =>
          (
            options.scoreExtractionRecipe( recipe ),
            recipe.producedIn.machineType.extractorType.flatMap( map.resourceNodes.get ).flatMap( _.get( product ) )
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
  ): Vector[ExtractionRecipe] = {
    ResourcePurity.values.reverse
      .fproduct( distrib.get )
      .flatMap {
        case ( purity, nodeCount ) =>
          Option.when( nodeCount > 0 )(
            configuredRecipe( recipe, options, purity ) match {
              case ( recipe, clockSpeed ) => ExtractionRecipe( recipe, clockSpeed, nodeCount )
            }
          )
      }
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
        displayName =
          show"${source.product.head.item.displayName} (${purity.entryName}, ${source.producedIn.displayName})",
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

  def apply( model: Model, recipeList: RecipeList, options: Options, map: MapOptions ): RecipeSelection = {
    val recipesWithCost = recipeList.recipes.fproduct( _.producedIn.powerConsumption / 10d )
    val extraction      = extractionRecipes( model, options, map )
    val tiered          = tieredExtractionRecipes( extraction, options )
    val caps            = resourceCaps( tiered )

    RecipeSelection(
      recipesWithCost,
      extraction,
      tiered,
      caps,
      caps.map { case ( item, cap ) => ( item, 1e6d / (cap * caps.size) ) }
    )

  }

}
