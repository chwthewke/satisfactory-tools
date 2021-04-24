package net.chwthewke.satisfactorytools
package prod

import model.Bill

trait Solver {
  def solve( bill: Bill, recipes: RecipeSelection ): Either[String, Solution]
}
