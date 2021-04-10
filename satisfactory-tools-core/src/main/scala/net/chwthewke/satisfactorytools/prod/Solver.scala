package net.chwthewke.satisfactorytools
package prod

trait Solver {
  def solve(
      bill: Bill,
      recipes: RecipeSelection,
      resourceWeights: ResourceWeights,
      resourceCaps: ResourceCaps
  ): Either[String, Solution]
}
