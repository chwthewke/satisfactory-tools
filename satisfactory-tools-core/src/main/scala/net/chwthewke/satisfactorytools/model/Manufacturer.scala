package net.chwthewke.satisfactorytools
package model

import cats.Show
import cats.syntax.show._
import io.circe.Decoder

final case class Manufacturer(
    className: ClassName,
    displayName: String,
    powerConsumption: Double
)

object Manufacturer {
  implicit val manufacturerDecoder: Decoder[Manufacturer] =
    Decoder.forProduct3(
      "ClassName",
      "mDisplayName",
      "mPowerConsumption"
    )(
      ( cn: ClassName, dn: String, pc: Double ) => Manufacturer( cn, dn, pc )
    )(
      Decoder[ClassName],
      Decoder[String],
      Decoders.doubleStringDecoder
    )

  implicit val manufacturerShow: Show[Manufacturer] = Show { manufacturer =>
    show"""${manufacturer.displayName} # ${manufacturer.className}
          |Power: ${f"${manufacturer.powerConsumption}%.0f"} MW
          |""".stripMargin
  }

}
