package net.chwthewke.satisfactorytools
package protocol

import cats.Order
import cats.Show
import cats.syntax.all._

case class PlanId( id: Int ) extends AnyVal

object PlanId {
  implicit val planIdShow: Show[PlanId] = Show[Int].contramap( _.id )

  implicit val planIdOrder: Order[PlanId]       = Order.by( _.id )
  implicit val planIdOrdering: Ordering[PlanId] = Order.catsKernelOrderingForOrder
}
