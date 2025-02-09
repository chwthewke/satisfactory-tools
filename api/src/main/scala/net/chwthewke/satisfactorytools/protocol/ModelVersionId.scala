package net.chwthewke.satisfactorytools
package protocol

import cats.Order
import cats.Show
import cats.syntax.all._

case class ModelVersionId( id: Int ) extends AnyVal

object ModelVersionId {
  implicit val modelVersionIdShow: Show[ModelVersionId]         = Show[Int].contramap( _.id )
  implicit val modelVersionIdOrder: Order[ModelVersionId]       = Order.by( _.id )
  implicit val modelVersionIdOrdering: Ordering[ModelVersionId] = Order.catsKernelOrderingForOrder
}
