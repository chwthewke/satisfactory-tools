package net.chwthewke.satisfactorytools
package model

import cats.Show
import cats.syntax.contravariant._
import io.circe.Decoder

final case class ClassName( name: String ) extends AnyVal

object ClassName {
  implicit val classNameDecoder: Decoder[ClassName] = Decoder[String].map( ClassName( _ ) )

  implicit val classNameShow: Show[ClassName] = Show[String].contramap( _.name )
}
