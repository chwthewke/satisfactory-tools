package net.chwthewke.factory
package prod

import cats.Order
import enumeratum.Enum
import enumeratum.EnumEntry

sealed abstract class Direction( val arrow: String ) extends EnumEntry

object Direction extends Enum[Direction] {
  object Provide extends Direction( "->" )
  object Receive extends Direction( "<-" )

  override val values: IndexedSeq[Direction] = findValues

  implicit val directionOrder: Order[Direction] = Order.reverse( Order.by( _.arrow ) )
}
