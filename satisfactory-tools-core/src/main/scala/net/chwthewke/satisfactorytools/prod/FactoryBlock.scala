package net.chwthewke.satisfactorytools
package prod

import cats.Order
import cats.Order.catsKernelOrderingForOrder
import cats.instances.double._
import cats.instances.list._
import cats.instances.sortedMap._
import cats.instances.sortedSet._
import cats.instances.string._
import cats.instances.tuple._
import cats.syntax.foldable._
import enumeratum.Enum
import enumeratum.EnumEntry
import mouse.boolean._
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet
//
import model.Countable
import model.Form
import model.Item
import model.Machine
import model.Recipe

final case class FactoryBlock( block: Countable[Recipe[Machine, Item], Double] ) {

  val machineCount: Int = block.amount.ceil.toInt
  val clockSpeed10: Int = (block.amount / machineCount * 1000).ceil.toInt

  val machine: Machine = block.item.producers.head
  val power: Double    = machineCount * machine.powerConsumption * math.pow( clockSpeed10 / 1000d, 1.6d )

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
      clockSpeed10,
      power
    )

  def inputsOutputs: SortedMap[Item, SortedSet[( FactoryBlock.Direction, String, Double )]] = {
    def line(
        direction: FactoryBlock.Direction,
        lineItem: Countable[Item, Double]
    ): SortedSet[( FactoryBlock.Direction, String, Double )] =
      SortedSet( ( direction, block.item.displayName, simpleItemAmount( lineItem ) ) )

    val ingredients =
      block.item.ingredientsPerMinute
        .foldMap( ci => SortedMap( ( ci.item, line( FactoryBlock.Direction.In, ci ) ) ) )
    val products =
      block.item.productsPerMinute
        .foldMap( ci => SortedMap( ( ci.item, line( FactoryBlock.Direction.Out, ci ) ) ) )

    ingredients ++ products
  }

}

object FactoryBlock {
  sealed abstract class Direction( val arrow: String ) extends EnumEntry
  object Direction extends Enum[Direction] {
    object Out extends Direction( "->" )
    object In  extends Direction( "<-" )

    override val values: IndexedSeq[Direction] = findValues

    implicit val directionOrder: Order[Direction] = Order.by( _.arrow )
  }
}
