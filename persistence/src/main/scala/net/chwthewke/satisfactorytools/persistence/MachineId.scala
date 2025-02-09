package net.chwthewke.satisfactorytools
package persistence

import cats.Order
import cats.Show
import cats.syntax.all._

final case class MachineId( id: Int ) extends AnyVal

object MachineId {
  implicit val showInstance: Show[MachineId]          = Show[Int].contramap( _.id )
  implicit val orderInstance: Order[MachineId]        = Order.by( _.id )
  implicit val machineIdOrdering: Ordering[MachineId] = Order.catsKernelOrderingForOrder
}
