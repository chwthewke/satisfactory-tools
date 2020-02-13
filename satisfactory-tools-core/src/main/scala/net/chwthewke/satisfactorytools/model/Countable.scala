package net.chwthewke.satisfactorytools.model

import cats.Show
import cats.instances.int._
import cats.syntax.show._

final case class Countable[N]( item: N, amount: Int )

object Countable {
  implicit def countableShow[N: Show]: Show[Countable[N]] =
    Show.show { case Countable( name, amount ) => show"$name ($amount)" }
}
