package net.chwthewke.satisfactorytools
package prod

import model.Bill

trait Solver[F[_]] {
  def solve( bill: Bill, recipes: RecipeSelection ): F[Solution]
}
