package net.chwthewke.satisfactorytools.model

import enumeratum.Circe
import enumeratum.Enum
import enumeratum.EnumEntry
import io.circe.Decoder

sealed abstract class Form( override val entryName: String ) extends EnumEntry

object Form extends Enum[Form] {
  final case object Solid   extends Form( "RF_SOLID" )
  final case object Liquid  extends Form( "RF_LIQUID" )
  final case object Invalid extends Form( "RF_INVALID" )

  override val values: IndexedSeq[Form] = findValues

  implicit val formDecoder: Decoder[Form] = Circe.decoder( this )
}
