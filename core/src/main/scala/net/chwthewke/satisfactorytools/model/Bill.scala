package net.chwthewke.satisfactorytools
package model

import cats.Eq
import cats.Show
import cats.syntax.all._

import data.Countable
import data.Item

final case class Bill( items: Vector[Countable[Double, Item]] ) {
  def amountOf( item: Item ): Double =
    items.find( _.item == item ).map( _.amount ).orEmpty
}

object Bill {

  val empty: Bill = Bill( Vector.empty )

  implicit val billShow: Show[Bill] = Show.show(
    _.items
      .map { case Countable( item, amount ) => f"$amount%.2f ${item.displayName}" }
      .mkString( "\n" )
  )

  implicit val billEq: Eq[Bill] =
    Eq.by( _.items.gather.sorted )

}
