package net.chwthewke.satisfactorytools
package data

import cats.Show
import enumeratum.Circe
import enumeratum.Enum
import enumeratum.EnumEntry
import io.circe.Decoder
import io.circe.Encoder

sealed abstract class Form( override val entryName: String, val simpleAmountFactor: Int ) extends EnumEntry

object Form extends Enum[Form] {
  final case object Solid   extends Form( "RF_SOLID", 1 )
  final case object Liquid  extends Form( "RF_LIQUID", 1000 )
  final case object Gas     extends Form( "RF_GAS", 1000 )
  final case object Invalid extends Form( "RF_INVALID", 1 )

  override val values: IndexedSeq[Form] = findValues

  implicit val formDecoder: Decoder[Form] = Circe.decoder( this )
  implicit val formEncoder: Encoder[Form] = Circe.encoder( this )
  implicit val formShow: Show[Form]       = Show.show( _.entryName )
}
