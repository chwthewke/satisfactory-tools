package net.chwthewke.satisfactorytools
package prod

import model.Countable
import model.Item
import model.Machine
import model.Recipe

case class Solution(
    recipes: Vector[Countable[Double, Recipe[Machine, Item]]],
    inputs: Vector[Countable[Double, Item]]
)
