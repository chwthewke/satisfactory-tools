package net.chwthewke.satisfactorytools
package model

import cats.Order
import cats.Show
import enumeratum.Circe
import enumeratum.Enum
import enumeratum.EnumEntry
import io.circe.Decoder
import io.circe.Encoder

sealed abstract class ManufacturerType( override val entryName: String, val description: String ) extends EnumEntry

object ManufacturerType extends Enum[ManufacturerType] {

  final case object Manufacturer extends ManufacturerType( "manufacturer", "Manufacturer" )
  final case object VariableManufacturer
      extends ManufacturerType( "variable-manufacturer", "Manufacturer (variable power)" )

  override val values: Vector[ManufacturerType] = findValues.toVector

  implicit val manufacturerTypeShow: Show[ManufacturerType]         = Show.show( _.description )
  implicit val manufacturerTypeOrder: Order[ManufacturerType]       = Order.by( values.indexOf )
  implicit val manufacturerTypeOrdering: Ordering[ManufacturerType] = Order.catsKernelOrderingForOrder

  implicit val manufacturerTypeDecoder: Decoder[ManufacturerType] = Circe.decoder( this )
  implicit val manufacturerTypeEncoder: Encoder[ManufacturerType] = Circe.encoder( this )
}
