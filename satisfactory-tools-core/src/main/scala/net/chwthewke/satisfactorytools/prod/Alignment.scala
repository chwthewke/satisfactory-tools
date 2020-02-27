package net.chwthewke.satisfactorytools
package prod

import enumeratum.Enum
import enumeratum.EnumEntry

sealed abstract class Alignment( val op: String => String ) extends EnumEntry with Product {
  def pad( text: String, width: Int ): String = op( op( text ).padTo( width, ' ' ) )
}

object Alignment extends Enum[Alignment] {
  final case object AlignLeft  extends Alignment( identity[String] )
  final case object AlignRight extends Alignment( _.reverse )

  override val values: IndexedSeq[Alignment] = findValues
}
