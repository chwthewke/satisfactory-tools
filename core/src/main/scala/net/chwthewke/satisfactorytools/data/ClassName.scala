package net.chwthewke.satisfactorytools
package data

import cats.Order
import cats.Show
import cats.syntax.contravariant._
import io.circe.Decoder
import io.circe.Encoder
import io.circe.KeyDecoder
import io.circe.KeyEncoder

final case class ClassName( name: String ) extends AnyVal

object ClassName {
  implicit val classNameShow: Show[ClassName]         = Show[String].contramap( _.name )
  implicit val classNameOrder: Order[ClassName]       = Order[String].contramap( _.name )
  implicit val classNameOrdering: Ordering[ClassName] = Order.catsKernelOrderingForOrder

  implicit val classNameDecoder: Decoder[ClassName] = Decoder[String].map( ClassName( _ ) )
  implicit val classNameEncoder: Encoder[ClassName] = Encoder[String].contramap( _.name )

  implicit val classNameKeyDecoder: KeyDecoder[ClassName] = KeyDecoder[String].map( ClassName( _ ) )
  implicit val classNameKeyEncoder: KeyEncoder[ClassName] = KeyEncoder[String].contramap( _.name )
}
