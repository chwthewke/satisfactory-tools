package net.chwthewke.dsptools
package prod

import net.chwthewke.factory.data.Countable
import net.chwthewke.factory.prod.SolverModule
import model.Item
import model.Recipe

object solver extends SolverModule {
  override type ItemId = Int
  override type ItemT  = Item

  override protected def itemId( item: Item ): Int = item.id

  override type RecipeId = Int
  override type RecipeT  = Recipe

  override protected def recipeId( recipe: Recipe ): Int                                   = recipe.id
  override protected def itemsPerMinute( recipe: Recipe ): Vector[Countable[Double, Item]] = recipe.itemsPerMinute

  object Solver extends GenericSolver

}
