package net.chwthewke.satisfactorytools
package protocol

import cats.Order
import cats.Show
import cats.syntax.contravariant._
import java.util.UUID

case class SessionId( id: UUID ) extends AnyVal

object SessionId {
  implicit val sessionIdShow: Show[SessionId] = Show[UUID].contramap( _.id )

  implicit val sessionIdOrder: Order[SessionId]       = Order.by( _.id )
  implicit val sessionIdOrdering: Ordering[SessionId] = Order.catsKernelOrderingForOrder
}
