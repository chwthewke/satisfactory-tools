package net.chwthewke.satisfactorytools
package data

import cats.Order
import cats.Show
import cats.syntax.show._
import io.circe.Decoder

import Parsers.ParserOps

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
  implicit val itemDecoder: Decoder[Item] =
    Decoder.forProduct6(
      "ClassName",
      "mDisplayName",
      "mForm",
      "mEnergyValue",
      "mResourceSinkPoints",
      "mSmallIcon"
    )(
      ( cn: ClassName, dn: String, fm: Form, ev: Double, pts: Option[Int], ico: IconData ) =>
        Item( cn, dn, fm, ev, pts.getOrElse( 0 ), ico )
    )(
      Decoder[ClassName],
      Decoder[String],
      Decoder[Form],
      Decoders.doubleStringDecoder,
      Decoder.decodeOption( Decoders.intStringDecoder ),
      Parsers.texture2d.decoder
    )

  implicit val showItem: Show[Item] = Show.show( item => show"""${item.displayName} # ${item.className}
                                                               |Form: ${item.form}
                                                               |Energy: ${item.energyValue} MJ
                                                               |Sink: ${item.sinkPoints} points
                                                               |""".stripMargin )

  implicit val itemOrder: Order[Item] = Order.by( _.displayName )
}
