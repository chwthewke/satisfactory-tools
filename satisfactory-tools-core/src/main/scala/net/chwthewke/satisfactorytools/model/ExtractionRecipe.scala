package net.chwthewke.satisfactorytools
package model

import data.Item

case class ExtractionRecipe( recipe: Recipe[Machine, Item], maxClockSpeed: Double, limit: Int )
