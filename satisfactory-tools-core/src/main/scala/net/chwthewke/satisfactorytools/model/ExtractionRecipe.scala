package net.chwthewke.satisfactorytools
package model

case class ExtractionRecipe( recipe: Recipe[Machine, Item], clockSpeed: Double, limit: Int )
