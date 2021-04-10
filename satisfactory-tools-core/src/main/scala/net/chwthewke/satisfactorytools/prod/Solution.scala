package net.chwthewke.satisfactorytools
package prod

import model.Countable
import model.Item
import model.Machine
import model.Recipe

case class Solution(
    recipes: Vector[Countable[Recipe[Machine, Item], Double]],
    inputs: Vector[Countable[Item, Double]]
)
