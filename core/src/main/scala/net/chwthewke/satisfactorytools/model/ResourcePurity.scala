package net.chwthewke.satisfactorytools
package model

import cats.Order
import cats.Show
import enumeratum.Circe
import enumeratum.Enum
import enumeratum.EnumEntry
import io.circe.Decoder
import io.circe.Encoder

sealed abstract class ResourcePurity( override val entryName: String, val multiplier: Double )
    extends EnumEntry
    with Product

object ResourcePurity extends Enum[ResourcePurity] {
  final case object Pure   extends ResourcePurity( "pure", 2.0d )
  final case object Normal extends ResourcePurity( "normal", 1.0d )
  final case object Impure extends ResourcePurity( "impure", 0.5d )

  override val values: Vector[ResourcePurity] = findValues.toVector

  implicit val resourcePurityShow: Show[ResourcePurity]   = Show.fromToString
  implicit val resourcePurityOrder: Order[ResourcePurity] = Order.by( indexOf )

  implicit val resourcePurityDecoder: Decoder[ResourcePurity] = Circe.decoder( this )
  implicit val resourcePurityEncoder: Encoder[ResourcePurity] = Circe.encoder( this )
}
