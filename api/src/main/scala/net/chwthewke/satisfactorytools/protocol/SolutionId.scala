package net.chwthewke.satisfactorytools
package protocol

import cats.Order
import cats.Show
import cats.syntax.all._

case class SolutionId( id: Int ) extends AnyVal

object SolutionId {
  implicit val solutionIdShow: Show[SolutionId]         = Show[Int].contramap( _.id )
  implicit val solutionIdOrder: Order[SolutionId]       = Order.by( _.id )
  implicit val solutionIdOrdering: Ordering[SolutionId] = Order.catsKernelOrderingForOrder
}
