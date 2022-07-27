package net.chwthewke.satisfactorytools
package protocol

import cats.Eq
import cats.Show
import cats.syntax.contravariant._

case class UserName( name: String ) extends AnyVal

object UserName {
  implicit val userNameShow: Show[UserName] = Show[String].contramap( _.name )
  implicit val userNameEq: Eq[UserName]     = Eq.by( _.name )
}
