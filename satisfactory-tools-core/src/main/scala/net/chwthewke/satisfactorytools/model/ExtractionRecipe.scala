package net.chwthewke.satisfactorytools
package model

case class ExtractionRecipe( recipe: Recipe[Machine, Item], maxClockSpeed: Double, limit: Int )
