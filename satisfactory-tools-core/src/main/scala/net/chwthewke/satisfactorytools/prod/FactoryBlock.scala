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

  def simpleItemAmount( ci: Countable[Item, Double] ): Double = {
    val factor = (ci.item.form == Form.Liquid).fold( 0.001, 1 )
    block.amount * ci.amount * factor
  }

  val firstItem: Countable[Item, Double] = block.item.productsPerMinute.head
  val itemAmount: Double                 = simpleItemAmount( firstItem )
  val itemAmountPerUnit: Double          = itemAmount / machineCount

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

  def ingredients: Map[Item, Vector[( String, Double )]] =
    block.item.ingredientsPerMinute
      .map( ci => ( ci.item, Vector( ( block.item.displayName, simpleItemAmount( ci ) ) ) ) )
      .to( Map )

}
