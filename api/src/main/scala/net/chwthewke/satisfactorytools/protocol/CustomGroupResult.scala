package net.chwthewke.satisfactorytools
package protocol

import data.Countable
import data.Item
import model.Machine
import prod.Factory

case class CustomGroupResult(
    index: Int,
    subFactory: Factory,
    items: Map[Item, ItemIO[ItemSrcDest.IntraGroup]],
    machines: Vector[Countable[Int, Machine]]
)
