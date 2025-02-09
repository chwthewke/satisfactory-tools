package net.chwthewke.satisfactorytools
package protocol

import cats.Order
import cats.Show
import cats.syntax.all._

case class UserId( id: Int ) extends AnyVal

object UserId {
  implicit val userIdShow: Show[UserId] = Show[Int].contramap( _.id )

  implicit val userIdOrder: Order[UserId]       = Order.by( _.id )
  implicit val userIdOrdering: Ordering[UserId] = Order.catsKernelOrderingForOrder
}
