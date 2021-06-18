package net.chwthewke.satisfactorytools
package model

import cats.Show
import cats.syntax.show._

import data.ClassName
import data.Extractor
import data.Manufacturer

case class Machine(
    className: ClassName,
    displayName: String,
    machineType: MachineType,
    powerConsumption: Double
)

object Machine {

  implicit val machineShow: Show[Machine] = Show.show {
    case Machine( className, displayName, machineType, powerConsumption ) =>
      show"""$displayName # $className
            |$machineType
            |Power: ${f"$powerConsumption%.0f MW"}""".stripMargin
  }

  def extractor( extractor: Extractor ): Either[String, Machine] =
    ExtractorType
      .fromExtractor( extractor )
      .toRight( s"No known extractor type for class ${extractor.className}, type ${extractor.extractorTypeName}" )
      .map(
        exType =>
          Machine(
            extractor.className,
            extractor.displayName,
            MachineType( exType ),
            extractor.powerConsumption
          )
      )

  def manufacturer( manufacturer: Manufacturer ): Machine =
    Machine(
      manufacturer.className,
      manufacturer.displayName,
      MachineType(
        if (manufacturer.powerConsumption == 0d) ManufacturerType.VariableManufacturer
        else ManufacturerType.Manufacturer
      ),
      manufacturer.powerConsumption
    )

}
