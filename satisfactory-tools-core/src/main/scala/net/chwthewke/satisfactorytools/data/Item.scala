package net.chwthewke.satisfactorytools
package data

import cats.Order
import cats.Show
import cats.syntax.apply._
import cats.syntax.show._
import io.circe.Decoder

final case class Item(
    className: ClassName,
    displayName: String,
    form: Form,
    energyValue: Double,
    sinkPoints: Int
) {
  def fuelValue: Double = energyValue * form.simpleAmountFactor

  override def toString: String = Item.showItem.show( this )
}

object Item {
  implicit val itemDecoder: Decoder[Item] = Decoder.instance(
    hc =>
      (
        hc.get[ClassName]( "ClassName" ),
        hc.get[String]( "mDisplayName" ),
        hc.get[Form]( "mForm" ),
        hc.get[Option[Double]]( "mEnergyValue" ),
        hc.get[Option[Int]]( "mResourceSinkPoints" )
      ).mapN( ( cn, dn, fm, ev, pts ) => Item( cn, dn, fm, ev.getOrElse( 0d ), pts.getOrElse( 0 ) ) )
  )

//  Decoder.forProduct5(
//    "ClassName",
//    "mDisplayName",
//    "mForm",
//    "mEnergyValue",
//    "mResourceSinkPoints"
//  )( ( cn: ClassName, dn: String, fm: Form, ev: Double, pts: Int ) => Item( cn, dn, fm, ev, pts ) )(
//    Decoder[ClassName],
//    Decoder[String],
//    Decoder[Form],
//    Decoders.doubleStringDecoder,
//    Decoders.intStringDecoder
//  )

  implicit val showItem: Show[Item] = Show.show( item => show"""${item.displayName} # ${item.className}
                                                               |Form: ${item.form}
                                                               |Energy: ${item.energyValue} MJ
                                                               |Sink: ${item.sinkPoints} points
                                                               |""".stripMargin )

  implicit val itemOrder: Order[Item] = Order.by( _.displayName )
}
