package net.chwthewke.satisfactorytools
package model

import cats.Order
import cats.Show
import cats.syntax.show._
import io.circe.Decoder

final case class Item(
    itemType: ItemType,
    className: ClassName,
    displayName: String,
    form: Form /* maybe not */,
    energyValue: Double
)

object Item {
  def itemDecoder( itemType: ItemType ): Decoder[Item] =
    Decoder.forProduct4(
      "ClassName",
      "mDisplayName",
      "mForm",
      "mEnergyValue"
    )( ( cn: ClassName, dn: String, fm: Form, ev: Double ) => Item( itemType, cn, dn, fm, ev ) )(
      Decoder[ClassName],
      Decoder[String],
      Decoder[Form],
      Decoders.doubleStringDecoder
    )

  implicit val showItem: Show[Item] = Show.show( item => show"""${item.displayName} # ${item.className}
                                                               |Type: ${item.itemType}
                                                               |Form: ${item.form}
                                                               |Energy: ${item.energyValue} MJ
                                                               |""".stripMargin )

  implicit val itemOrder: Order[Item] = Order.by( _.displayName )
}
