package net.chwthewke.satisfactorytools
package protocol

import cats.Eq
import cats.Show
import cats.syntax.contravariant._

case class PlanName( name: String ) extends AnyVal {
  def copy: PlanName = PlanName( name + " (copy)" )
}

object PlanName {
  implicit val planNameShow: Show[PlanName] = Show[String].contramap( _.name )
  implicit val planNameEq: Eq[PlanName]     = Eq.by( _.name )
}
