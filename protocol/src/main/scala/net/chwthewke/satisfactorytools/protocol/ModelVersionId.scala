package net.chwthewke.satisfactorytools
package protocol

import cats.Order
import cats.Show
import cats.syntax.contravariant._
import io.circe.Decoder
import io.circe.Encoder

case class ModelVersionId( id: Int ) extends AnyVal

object ModelVersionId {
  implicit val modelVersionIdShow: Show[ModelVersionId]   = Show[Int].contramap( _.id )
  implicit val modelVersionIdOrder: Order[ModelVersionId] = Order.by( _.id )

  implicit val modelVersionIdDecoder: Decoder[ModelVersionId] = Decoder[Int].map( ModelVersionId( _ ) )
  implicit val modelVersionIdEncoder: Encoder[ModelVersionId] = Encoder[Int].contramap( _.id )
}
