package net.chwthewke.satisfactorytools
package prod

import cats.Order
import enumeratum.Enum
import enumeratum.EnumEntry

sealed abstract class Direction( val arrow: String ) extends EnumEntry

object Direction extends Enum[Direction] {
  object Out extends Direction( "->" )
  object In  extends Direction( "<-" )

  override val values: IndexedSeq[Direction] = findValues

  implicit val directionOrder: Order[Direction] = Order.by( _.arrow )
}
