package net.chwthewke.satisfactorytools
package model

import cats.Show
import cats.instances.string._
import cats.syntax.show._

case class Machine(
    className: ClassName,
    displayName: String,
    powerConsumption: Double
)

object Machine {
  implicit val machineShow: Show[Machine] = Show.show {
    case Machine( className, displayName, powerConsumption ) =>
      show"""$displayName # $className
            |Power: ${f"$powerConsumption%.0f MW"}""".stripMargin
  }

  def extractor( extractor: Extractor ): Machine =
    Machine(
      extractor.className,
      extractor.displayName,
      extractor.powerConsumption
    )

  def manufacturer( manufacturer: Manufacturer ): Machine =
    Machine(
      manufacturer.className,
      manufacturer.displayName,
      manufacturer.powerConsumption
    )

}
