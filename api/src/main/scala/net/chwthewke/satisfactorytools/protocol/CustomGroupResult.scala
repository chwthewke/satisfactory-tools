package net.chwthewke.satisfactorytools
package protocol

import data.ClassName
import data.Countable
import data.Item
import model.Machine
import prod.Factory
import prod.GroupAssignment

case class CustomGroupResult(
    index: Int,
    subFactory: Factory,
    groupAssignment: GroupAssignment[ClassName],
    items: Map[Item, ItemIO[ItemSrcDest.IntraGroup]],
    machines: Vector[Countable[Int, Machine]]
)
