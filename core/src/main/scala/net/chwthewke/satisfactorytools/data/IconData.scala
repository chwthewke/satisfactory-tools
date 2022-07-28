package net.chwthewke.satisfactorytools
package data

import cats.Show
import cats.derived.semiauto
import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

case class IconData( dir: String, packageName: String, textureName: String )

object IconData {
  implicit val iconDataShow: Show[IconData]       = semiauto.show[IconData]
  implicit val iconDataDecoder: Decoder[IconData] = deriveDecoder[IconData]
  implicit val iconDataEncoder: Encoder[IconData] = deriveEncoder[IconData]
}
