package net.chwthewke.satisfactorytools
package data

import cats.Order
import cats.Show
import cats.syntax.show._
import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

final case class Item(
    className: ClassName,
    displayName: String,
    form: Form,
    energyValue: Double,
    sinkPoints: Int,
    smallIcon: IconData
) {
  def fuelValue: Double = energyValue * form.simpleAmountFactor

  override def toString: String = Item.showItem.show( this )
}

object Item {

  implicit val showItem: Show[Item] = Show.show( item => show"""${item.displayName} # ${item.className}
                                                               |Form: ${item.form}
                                                               |Energy: ${item.energyValue} MJ
                                                               |Sink: ${item.sinkPoints} points
                                                               |Icon: ${item.smallIcon}
                                                               |""".stripMargin )

  implicit val itemOrder: Order[Item]       = Order.by( _.displayName )
  implicit val itemOrdering: Ordering[Item] = Order.catsKernelOrderingForOrder

  implicit val itemDecoder: Decoder[Item] = deriveDecoder[Item]
  implicit val itemEncoder: Encoder[Item] = deriveEncoder[Item]
}
