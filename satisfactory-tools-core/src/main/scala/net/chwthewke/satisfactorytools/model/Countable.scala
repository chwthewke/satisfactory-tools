package net.chwthewke.satisfactorytools
package model

import cats.Show
import cats.syntax.show._

final case class Countable[N, A]( item: N, amount: A )

object Countable {
  implicit def countableShow[N: Show, A: Show]: Show[Countable[N, A]] =
    Show.show { case Countable( name, amount ) => show"$name ($amount)" }
}
