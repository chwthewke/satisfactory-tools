package net.chwthewke.satisfactorytools
package prod

import mouse.boolean._
//
import model.Countable
import model.Form
import model.Item
import model.Machine
import model.Recipe

final case class FactoryBlock( block: Countable[Recipe[Machine, Item], Double] ) {

  val machineCount: Int = block.amount.ceil.toInt
  val clockSpeed: Int   = (block.amount / machineCount * 100).ceil.toInt

  val machine: Machine = block.item.producers.head
  val power: Double    = machineCount * machine.powerConsumption * math.pow( clockSpeed / 100d, 1.6d )

  val firstItem: Countable[Item, Double] = block.item.productsPerMinute.head
  val itemFactor: Double                 = (firstItem.item.form == Form.Liquid).fold( 0.001, 1 )
  val itemAmount: Double                 = block.amount * firstItem.amount * itemFactor
  val itemAmountPerUnit: Double          = itemAmount / machineCount * itemFactor

  def tableColumns: Vector[String] =
    FactoryTable.columnsForBlock(
      block.item.displayName,
      itemAmount,
      itemAmountPerUnit,
      machine.displayName,
      machineCount,
      clockSpeed,
      power
    )

}
