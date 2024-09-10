package net.chwthewke.satisfactorytools
package model

import cats.Order
import cats.Show
import cats.derived.semiauto
import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

case class ModelVersion( version: Int, name: String )

object ModelVersion {
  implicit val modelVersionShow: Show[ModelVersion]         = semiauto.show[ModelVersion]
  implicit val modelVersionOrder: Order[ModelVersion]       = Order.by( _.version )
  implicit val modelVersionOrdering: Ordering[ModelVersion] = Order.catsKernelOrderingForOrder

  implicit val modelVersionDecoder: Decoder[ModelVersion] = deriveDecoder[ModelVersion]
  implicit val modelVersionEncoder: Encoder[ModelVersion] = deriveEncoder[ModelVersion]
}
