package net.chwthewke.satisfactorytools

import cats.Show
import cats.instances.string._
import cats.syntax.contravariant._

final case class ClassName( name: String ) extends AnyVal

object ClassName {
  implicit val classNameShow: Show[ClassName] = Show[String].contramap( _.name )
}
