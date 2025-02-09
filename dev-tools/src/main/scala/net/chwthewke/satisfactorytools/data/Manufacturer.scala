package net.chwthewke.satisfactorytools
package data

import cats.Show
import cats.syntax.show._
import io.circe.Decoder

final case class Manufacturer(
    className: ClassName,
    displayName: String,
    powerConsumption: Double,
    powerConsumptionExponent: Double,
    isCollider: Boolean
)

object Manufacturer {
  def manufacturerDecoder( isCollider: Boolean ): Decoder[Manufacturer] =
    Decoder.forProduct4(
      "ClassName",
      "mDisplayName",
      "mPowerConsumption",
      "mPowerConsumptionExponent"
    )( ( cn, dn, pc, pe ) => Manufacturer( cn, dn, pc, pe, isCollider ) )(
      Decoder[ClassName],
      Decoder[String],
      Decoders.doubleStringDecoder,
      Decoders.doubleStringDecoder
    )

  implicit val manufacturerShow: Show[Manufacturer] = Show { manufacturer =>
    show"""${manufacturer.displayName} # ${manufacturer.className}
          |Power: ${f"${manufacturer.powerConsumption}%.0f"} MW (exp: ${f"${manufacturer.powerConsumptionExponent}%.4f"})
          |""".stripMargin
  }

}
