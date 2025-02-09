package net.chwthewke.satisfactorytools
package prod

import cats.syntax.all._

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
    tieredExtractionRecipes: Map[Item, List[ExtractionRecipe]],
    resourceCaps: Map[Item, Double],
    resourceWeights: Map[Item, Double]
)

object RecipeSelection {

  def tieredExtractionRecipes(
      options: Options,
      map: ResourceOptions,
      extractionRecipes: Vector[( Item, ResourcePurity, Recipe )]
  ): Map[Item, List[ExtractionRecipe]] =
    extractionRecipes
      .foldMap {
        case ( product, purity, recipe ) =>
          (
            options.scoreExtractionRecipe( recipe ),
            recipe.producedIn.machineType.extractorType
              .flatMap( map.resourceNodes.get )
              .flatMap( _.get( product.className ) )
              .map( _.get( purity ) )
          ).mapN( ( score, nodes ) => Map( ( product, Vector( ( ( score, purity ), ( nodes, recipe ) ) ) ) ) ).orEmpty
      }
      .map {
        case ( item, recipes ) =>
          (
            item,
            recipes
              .sortBy( _._1 )
              .map { case ( _, ( max, recipe ) ) => extractionRecipe( item, max, recipe, options ) }
              .toList
          )
      }

  def extractionRecipe(
      item: Item,
      limit: Int,
      source: Recipe,
      options: Options
  ): ExtractionRecipe = {
    val clockSpeed: Double = source.productsPerMinute.map {
      case Countable( item, amountPerMinute ) =>
        val maxAmountPerMinute: Int =
          if (item.form == Form.Solid) options.belt.itemsPerMinute else options.pipe.cubicMetersPerMinute

        options.clockSpeed.percent.toDouble.min( 100d * maxAmountPerMinute / amountPerMinute )
    }.minimum

    val maxAmountPerExtractor: Double =
      source.productsPerMinute
        .find( _.item == item )
        .fold( 0d )( _.amount * clockSpeed / 100d )

    ExtractionRecipe( source, clockSpeed, limit, maxAmountPerExtractor )
  }

  def resourceCaps( tieredExtractionRecipes: Map[Item, List[ExtractionRecipe]] ): Map[Item, Double] =
    tieredExtractionRecipes
      .map { case ( item, tiers ) => ( item, tiers.foldMap( _.maxAmount ) ) }

  def apply(
      model: Model,
      recipeList: RecipeList,
      options: Options,
      resourceOpts: ResourceOptions
  ): RecipeSelection = {
    val recipesWithCost  = recipeList.recipes.fproduct( _.power.average / 1000000 )
    val tieredExtraction = tieredExtractionRecipes( options, resourceOpts, model.extractionRecipes )
    val extraction       = tieredExtraction.keys.toVector
    val caps             = resourceCaps( tieredExtraction )

    RecipeSelection(
      recipesWithCost,
      extraction,
      tieredExtraction,
      caps,
      resourceOpts.resourceWeights.costs( caps )
    )
  }

  def apply( model: Model, inputs: SolverInputs ): RecipeSelection =
    apply( model, inputs.recipeList, inputs.options, inputs.resourceOptions )

}
