package net.chwthewke.satisfactorytools
package prod

import cats.Order.catsKernelOrderingForOrder
import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.reducible._

import data.ClassName
import data.Countable
import data.Form
import data.Item
import model.ExtractionRecipe
import model.Model
import model.Options
import model.Recipe
import model.RecipeList
import model.ResourceOptions
import model.ResourcePurity

case class RecipeSelection(
    allowedRecipes: Vector[( Recipe, Double )],
    extractedItems: Vector[Item],
    tieredExtractionRecipes: Map[Item, Vector[ExtractionRecipe]],
    resourceCaps: Map[Item, Double],
    resourceWeights: Map[Item, Double]
)

object RecipeSelection {

  val nuclearWasteClassName: ClassName = ClassName( "Desc_NuclearWaste_C" )

  def tieredExtractionRecipes(
      options: Options,
      map: ResourceOptions,
      extractionRecipes: Vector[( Item, ResourcePurity, Recipe )]
  ): Map[Item, Vector[ExtractionRecipe]] =
    extractionRecipes
      .foldMap {
        case ( product, purity, recipe ) =>
          (
            options.scoreExtractionRecipe( recipe ),
            recipe.producedIn.machineType.extractorType
              .flatMap( map.resourceNodes.get )
              .flatMap( _.get( product ) )
              .map( _.get( purity ) )
          ).mapN( ( score, nodes ) => Map( ( product, Vector( ( ( score, purity ), ( nodes, recipe ) ) ) ) ) ).orEmpty
      }
      .map {
        case ( item, recipes ) =>
          (
            item,
            recipes.sortBy( _._1 ).map { case ( _, ( max, recipe ) ) => extractionRecipe( max, recipe, options ) }
          )
      }

  def extractionRecipe(
      limit: Int,
      source: Recipe,
      options: Options
  ): ExtractionRecipe = {
    val clockSpeed = source.products.map {
      case Countable( item, amount ) =>
        val maxAmountPerMinute: Int =
          if (item.form == Form.Solid) options.belt.itemsPerMinute else options.pipe.cubicMetersPerMinute

        val maxOutput: Double =
          maxAmountPerMinute.toDouble / 60000d * source.duration.toMillis

        options.clockSpeed.percent.toDouble.min( 100d * maxOutput / amount )
    }.minimum

    ExtractionRecipe( source, clockSpeed, limit )
  }

  def resourceCaps( tieredExtractionRecipes: Map[Item, Vector[ExtractionRecipe]] ): Map[Item, Double] =
    tieredExtractionRecipes.map {
      case ( item, tiers ) =>
        ( item, tiers.foldMap {
          case ExtractionRecipe( recipe, clock, count ) =>
            recipe.productsPerMinute.find( _.item == item ).fold( 0d )( _.amount * count * clock / 100d )
        } )
    }

  def apply(
      model: Model,
      recipeList: RecipeList,
      options: Options,
      resourceOpts: ResourceOptions
  ): RecipeSelection = {
    val recipesWithCost  = recipeList.recipes.fproduct( _.power.average )
    val tieredExtraction = tieredExtractionRecipes( options, resourceOpts, model.extractionRecipes )
    val extraction       = tieredExtraction.keys.toVector
    val caps             = resourceCaps( tieredExtraction )

    RecipeSelection(
      recipesWithCost,
      extraction ++ model.items.get( nuclearWasteClassName ),
      tieredExtraction,
      caps,
      resourceOpts.resourceWeights.costs( caps )
    )
  }

  def apply( model: Model, inputs: SolverInputs ): RecipeSelection =
    apply( model, inputs.recipeList, inputs.options, inputs.resourceOptions )

}
