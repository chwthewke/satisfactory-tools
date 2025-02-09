package net.chwthewke.satisfactorytools
package protocol

import cats.Eq
import cats.Show
import cats.syntax.all._

import model.ModelVersion

case class PlanName( name: String ) extends AnyVal {
  def copy: PlanName                         = PlanName( name + " (copy)" )
  def migrated( to: ModelVersion ): PlanName = PlanName( name + s" (migrated to ${to.name})" )
}

object PlanName {
  implicit val planNameShow: Show[PlanName] = Show[String].contramap( _.name )
  implicit val planNameEq: Eq[PlanName]     = Eq.by( _.name )
}
