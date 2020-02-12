package net.chwthewke.satisfactorytools

import cats.Show
import cats.instances.int._
import cats.syntax.show._

final case class Countable[N]( name: N, amount: Int )

object Countable {
  implicit def countableShow[N: Show]: Show[Countable[N]] =
    Show.show { case Countable( name, amount ) => show"$name ($amount)" }
}
