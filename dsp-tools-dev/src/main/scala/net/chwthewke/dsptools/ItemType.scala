package net.chwthewke.dsptools

import enumeratum.CatsEnum
import enumeratum.Enum
import enumeratum.EnumEntry
import scodec.Decoder

sealed abstract class ItemType extends EnumEntry

object ItemType extends Enum[ItemType] with CatsEnum[ItemType] with Decoders {
  case object Unknown    extends ItemType
  case object Resource   extends ItemType
  case object Material   extends ItemType
  case object Component  extends ItemType
  case object Product    extends ItemType
  case object Logistics  extends ItemType
  case object Production extends ItemType
  case object Decoration extends ItemType
  case object Weapon     extends ItemType
  case object Matrix     extends ItemType
  case object Monster    extends ItemType

  override val values: Vector[ItemType] = findValues.toVector

  implicit lazy val decoder: Decoder[ItemType] = decodeEnum( this )
}
