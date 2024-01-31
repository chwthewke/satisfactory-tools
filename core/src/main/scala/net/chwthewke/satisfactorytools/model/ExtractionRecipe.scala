package net.chwthewke.satisfactorytools
package model

import cats.Show

case class ExtractionRecipe( recipe: Recipe, maxClockSpeed: Double, limit: Int, maxAmountPerExtractor: Double ) {
  val maxAmount: Double = maxAmountPerExtractor * limit
}

object ExtractionRecipe {
  implicit val extractionRecipeShow: Show[ExtractionRecipe] = Show.show {
    case ExtractionRecipe( recipe, maxClockSpeed, limit, maxAmountPerExtractor ) =>
      f"$limit%d ${recipe.displayName} @ $maxClockSpeed%3.0f %% max, logistic bound $maxAmountPerExtractor%.0f"
  }

}
