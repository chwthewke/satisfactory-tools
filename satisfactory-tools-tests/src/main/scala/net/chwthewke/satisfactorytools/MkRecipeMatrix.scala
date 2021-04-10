package net.chwthewke.satisfactorytools

import data.ProductionConfig
import model.Item
import model.Machine
import model.Model
import model.Recipe
import prod.RecipeMatrix

object MkRecipeMatrix {
  def apply( productionConfig: ProductionConfig, model: Model ): RecipeMatrix = {
    val activeRecipes: Vector[Recipe[Machine, Item]] =
      productionConfig.recipes
        .flatMap(
          cn =>
            model.manufacturingRecipes
              .find( _.className == cn )
              .orElse( model.extractionRecipes.map( _._2 ).find( _.className == cn ) )
        )
    val wantedItems: Vector[Item] =
      productionConfig.items
        .filter( _.amount != 0d )
        .flatMap( ci => model.items.get( ci.item ) )

    RecipeMatrix.init( activeRecipes, wantedItems )
  }

}
