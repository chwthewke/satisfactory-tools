package net.chwthewke.satisfactorytools
package model

import cats.Order
import cats.Show
import enumeratum.Enum
import enumeratum.EnumEntry

sealed abstract class ResourcePurity( override val entryName: String, val multiplier: Double ) extends EnumEntry

object ResourcePurity extends Enum[ResourcePurity] {
  final case object Pure   extends ResourcePurity( "pure", 2.0d )
  final case object Normal extends ResourcePurity( "normal", 1.0d )
  final case object Impure extends ResourcePurity( "impure", 0.5d )

  override val values: Vector[ResourcePurity] = findValues.toVector

  implicit val resourcePurityShow: Show[ResourcePurity]   = Show.fromToString
  implicit val resourcePurityOrder: Order[ResourcePurity] = Order.by( indexOf )
}
