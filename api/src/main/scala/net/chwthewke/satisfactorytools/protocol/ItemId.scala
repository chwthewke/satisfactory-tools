package net.chwthewke.satisfactorytools
package protocol

import cats.Order
import cats.Show
import cats.syntax.contravariant._

final case class ItemId( id: Int ) extends AnyVal

object ItemId {
  implicit val showInstance: Show[ItemId]       = Show[Int].contramap( _.id )
  implicit val orderInstance: Order[ItemId]     = Order.by( _.id )
  implicit val itemIdOrdering: Ordering[ItemId] = Order.catsKernelOrderingForOrder
}
