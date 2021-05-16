package net.chwthewke.satisfactorytools
package prod

import data.Countable
import data.Item
import model.Machine
import model.Recipe

case class Solution(
    recipes: Vector[Countable[Double, Recipe[Machine, Item]]],
    inputs: Vector[Countable[Double, Item]]
)
