package net.chwthewke.satisfactorytools
package model

import cats.Show
import cats.syntax.show._

final case class Countable[N, A]( item: N, amount: A ) {
  def simpleAmount( implicit ev: N =:= Item, numeric: Fractional[A] ): A =
    numeric.div( amount, Countable.simpleAmountFactor( ev( item ) ) )
}

object Countable {
  def simpleAmountFactor[A]( item: Item )( implicit numeric: Fractional[A] ): A =
    numeric.fromInt( item.form.simpleAmountFactor )

  def fromSimpleAmount[A]( item: Item, amount: A )( implicit numeric: Fractional[A] ): Countable[Item, A] =
    Countable( item, numeric.times( amount, simpleAmountFactor( item ) ) )

  implicit def countableShow[N: Show, A: Show]: Show[Countable[N, A]] =
    Show.show { case Countable( name, amount ) => show"$name ($amount)" }
}
