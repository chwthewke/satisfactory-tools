package net.chwthewke.satisfactorytools
package prod

import data.Countable
import data.Item
import model.Recipe

final case class Factory(
    extraction: Vector[ClockedRecipe],
    manufacturing: Vector[Countable[Double, Recipe]],
    extraInputs: Vector[Countable[Double, Item]],
    extraOutputs: Vector[Countable[Double, Item]]
) {

  def allRecipes: Vector[ClockedRecipe] = extraction ++ manufacturing.map( ClockedRecipe.roundUp )

}
