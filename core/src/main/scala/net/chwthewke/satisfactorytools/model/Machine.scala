package net.chwthewke.satisfactorytools
package model

import cats.Show
import cats.syntax.all._
import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

import data.ClassName

case class Machine(
    className: ClassName,
    displayName: String,
    machineType: MachineType,
    powerConsumption: Double,
    powerConsumptionExponent: Double
)

object Machine {

  implicit val machineShow: Show[Machine] = Show.show {
    case Machine( className, displayName, machineType, powerConsumption, powerConsumptionExponent ) =>
      show"""$displayName # $className
            |$machineType
            |Power: ${f"$powerConsumption%.0f MW"} (exp: ${f"$powerConsumptionExponent%.4f"})""".stripMargin
  }

  implicit val machineDecoder: Decoder[Machine] = deriveDecoder[Machine]
  implicit val machineEncoder: Encoder[Machine] = deriveEncoder[Machine]
}
