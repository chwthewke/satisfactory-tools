package net.chwthewke.satisfactorytools
package model

import enumeratum.Enum
import enumeratum.EnumEntry

sealed abstract class ResourcePurity( override val entryName: String, val value: Double ) extends EnumEntry

object ResourcePurity extends Enum[ResourcePurity] {
  final case object Impure extends ResourcePurity( "impure", 0.5d )
  final case object Normal extends ResourcePurity( "normal", 1.0d )
  final case object Pure   extends ResourcePurity( "pure", 2.0d )

  override val values: Vector[ResourcePurity] = findValues.toVector
}
