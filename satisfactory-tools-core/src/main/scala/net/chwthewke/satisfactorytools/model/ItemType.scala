package net.chwthewke.satisfactorytools
package model

import cats.Show
import enumeratum.Enum
import enumeratum.EnumEntry

sealed abstract class ItemType extends EnumEntry

object ItemType extends Enum[ItemType] {

  final case object Part        extends ItemType
  final case object Resource    extends ItemType
  final case object Biomass     extends ItemType
  final case object NuclearFuel extends ItemType
  final case object Consumable  extends ItemType

  override val values: IndexedSeq[ItemType] = findValues

  implicit val itemTypeShow: Show[ItemType] = Show( _.entryName )
}
