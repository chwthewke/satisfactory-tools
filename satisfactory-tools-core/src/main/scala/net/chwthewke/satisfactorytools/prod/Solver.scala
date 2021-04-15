package net.chwthewke.satisfactorytools
package prod

trait Solver {
  def solve( bill: Bill, recipes: RecipeSelection ): Either[String, Solution]
}
