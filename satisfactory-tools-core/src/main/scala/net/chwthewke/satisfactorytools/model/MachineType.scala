package net.chwthewke.satisfactorytools
package model

import enumeratum.Enum
import enumeratum.EnumEntry

sealed abstract class MachineType( override val entryName: String ) extends EnumEntry

object MachineType extends Enum[MachineType] {
  final case object Manufacturer extends MachineType( "manufacturer" )
  final case object Extractor    extends MachineType( "extractor" )

  override def values: IndexedSeq[MachineType] = findValues
}
