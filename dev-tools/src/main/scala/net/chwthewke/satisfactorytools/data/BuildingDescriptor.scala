package net.chwthewke.satisfactorytools
package data

import cats.Show
import cats.derived.semiauto
import cats.syntax.all._
import io.circe.Decoder

import Parsers._

case class BuildingDescriptor(
    className: ClassName,
    smallIcon: Option[IconData]
)

object BuildingDescriptor {
  implicit val buildingDescriptorShow: Show[BuildingDescriptor] = semiauto.show[BuildingDescriptor]

  implicit val buildingDescriptorDecoder: Decoder[BuildingDescriptor] =
    Decoder.forProduct2( "ClassName", "mSmallIcon" )( BuildingDescriptor.apply )(
      Decoder[ClassName],
      texture2d.decoder.map( _.some ).or( Decoder.const( none ) )
    )
}
