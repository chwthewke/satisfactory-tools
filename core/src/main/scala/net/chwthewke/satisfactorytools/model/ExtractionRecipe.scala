package net.chwthewke.satisfactorytools
package model

case class ExtractionRecipe( recipe: Recipe, maxClockSpeed: Double, limit: Int, maxAmountPerExtractor: Double ) {
  val maxAmount: Double = maxAmountPerExtractor * limit
}
