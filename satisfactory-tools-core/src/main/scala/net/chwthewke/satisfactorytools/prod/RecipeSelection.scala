package net.chwthewke.satisfactorytools
package prod

import cats.Order.catsKernelOrderingForOrder
import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.reducible._

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
import model.ResourcePurity
import model.SolverInputs

case class RecipeSelection(
    allowedRecipes: Vector[( Recipe[Machine, Item], Double )],
    extractedItems: Vector[Item],
    tieredExtractionRecipes: Map[Item, Vector[ExtractionRecipe]],
    resourceCaps: Map[Item, Double],
    resourceWeights: Map[Item, Double]
)

object RecipeSelection {
  def tieredExtractionRecipes(
      options: Options,
      map: MapOptions,
      extractionRecipes: Vector[( Item, ResourcePurity, Recipe[Machine, Item] )]
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
      source: Recipe[Machine, Item],
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

  def apply( model: Model, recipeList: RecipeList, options: Options, map: MapOptions ): RecipeSelection = {
    val recipesWithCost  = recipeList.recipes.fproduct( _.producedIn.powerConsumption / 10d )
    val tieredExtraction = tieredExtractionRecipes( options, map, model.extractionRecipes )
    val extraction       = tieredExtraction.keys.toVector
    val caps             = resourceCaps( tieredExtraction )

    RecipeSelection(
      recipesWithCost,
      extraction,
      tieredExtraction,
      caps,
      caps.map { case ( item, cap ) => ( item, 1e6d / (cap * caps.size) ) }
    )
  }

  def apply( model: Model, inputs: SolverInputs ): RecipeSelection =
    apply( model, inputs.recipeList, inputs.options, inputs.mapOptions )

}
