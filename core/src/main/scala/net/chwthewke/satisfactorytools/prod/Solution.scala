package net.chwthewke.satisfactorytools
package prod

import data.Countable
import data.Item
import model.Recipe

case class Solution(
    recipes: Vector[Countable[Double, Recipe]],
    inputs: Vector[Countable[Double, Item]]
)
