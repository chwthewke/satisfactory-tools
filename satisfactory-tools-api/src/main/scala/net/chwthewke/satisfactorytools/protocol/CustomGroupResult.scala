package net.chwthewke.satisfactorytools
package protocol

import data.Countable
import data.Item
import model.Machine
import prod.Factory

case class CustomGroupResult(
    subFactory: Factory,
    items: Map[Item, ItemIO],
    machines: Vector[Countable[Int, Machine]]
)
