package net.chwthewke.satisfactorytools.model

import io.circe.Decoder

final case class Item(
    itemType: ItemType,
    className: ClassName,
    displayName: String,
    form: Form /* maybe not */,
    energyValue: Double,
    resourceSinkPoints: Int
)

object Item {
  def itemDecoder( itemType: ItemType ): Decoder[Item] =
    Decoder.forProduct5(
      "ClassName",
      "mDisplayName",
      "mForm",
      "mEnergyValue",
      "mResourceSinkPoints"
    )( ( cn: ClassName, dn: String, fm: Form, ev: Double, sp: Int ) => Item( itemType, cn, dn, fm, ev, sp ) )(
      Decoder[ClassName],
      Decoder[String],
      Decoder[Form],
      Decoders.doubleStringDecoder,
      Decoders.intStringDecoder
    )
}
