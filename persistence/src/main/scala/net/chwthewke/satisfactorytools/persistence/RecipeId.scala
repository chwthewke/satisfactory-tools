package net.chwthewke.satisfactorytools
package persistence

import cats.Show
import cats.kernel.Order
import cats.syntax.all._

final case class RecipeId( id: Int ) extends AnyVal

object RecipeId {
  implicit val recipeIdShow: Show[RecipeId]         = Show[Int].contramap( _.id )
  implicit val recipeIdOrder: Order[RecipeId]       = Order.by( _.id )
  implicit val recipeIdOrdering: Ordering[RecipeId] = Order.catsKernelOrderingForOrder
}
