package net.chwthewke.satisfactorytools
package persistence

import cats.Show
import cats.kernel.Order
import cats.syntax.contravariant._

final case class RecipeId( id: Int ) extends AnyVal

object RecipeId {
  implicit val showInstance: Show[RecipeId]   = Show[Int].contramap( _.id )
  implicit val orderInstance: Order[RecipeId] = Order.by( _.id )
}
