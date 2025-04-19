package net.chwthewke.satisfactorytools
package data

import cats.Show
import cats.syntax.all._
import io.circe.Decoder

case class PowerGenerator(
    className: ClassName,
    displayName: String,
    powerProduction: Double, // MW
    powerConsumptionExponent: Double,
    supplementalToPowerRatio: Double, // L/MJ
    fuels: Vector[PowerGenerator.GeneratorFuel]
)

object PowerGenerator {

  implicit val powerGeneratorShow: Show[PowerGenerator] = Show { generator =>
    show"""${generator.displayName} # ${generator.className}
          |Power: ${f"${generator.powerProduction}%.0f"} MW (exp: ${f"${generator.powerConsumptionExponent}%.4f"})
          |Fuels:
          |  ${generator.fuels.sortBy( _.fuel ).mkString_( "\n  " )}
          |Suppl. resource to power ratio: ${f"${generator.supplementalToPowerRatio}%.4f"}  
          |""".stripMargin
  }

  private case class ByproductAmount( amount: Int ) extends AnyVal
  private object ByproductAmount {
    implicit val byproductAmountDecoder: Decoder[ByproductAmount] = Decoder
      .instance( hc =>
        hc.as[String] match {
          case Right( "" ) => Right( 0 )
          case _           => hc.as[Int]
        }
      )
      .map( ByproductAmount( _ ) )
  }

  case class GeneratorFuel(
      fuel: ClassName,
      byproduct: Option[Countable[Int, ClassName]],
      supplementalResource: Option[ClassName]
  )

  object GeneratorFuel {
    implicit val generatorFuelShow: Show[GeneratorFuel] = Show.show { fuel =>
      show"${fuel.fuel}${fuel.supplementalResource.map( cn => show" (with $cn)" ).orEmpty}${fuel.byproduct.map( cn => show" -> $cn" ).orEmpty}"
    }
  }

  private implicit val fuelsDecoder: Decoder[Vector[GeneratorFuel]] = {
    implicit val supplementalResourceClassDecoder: Decoder[Option[ClassName]] =
      Decoder.decodeString.map { str => Option.when( str.nonEmpty )( ClassName( str ) ) }
    implicit val fuelDecoder: Decoder[GeneratorFuel] =
      Decoder.forProduct4[GeneratorFuel, ClassName, Option[ClassName], ByproductAmount, Option[ClassName]](
        "mFuelClass",
        "mByproduct",
        "mByproductAmount",
        "mSupplementalResourceClass"
      )( ( f, p, a, s ) => GeneratorFuel( f, p.map( Countable( _, a.amount ) ), s ) )

    Decoder.decodeVector[GeneratorFuel]
  }

  implicit val powerGeneratorDecoder: Decoder[PowerGenerator] =
    Decoder.forProduct6[PowerGenerator, ClassName, String, Double, Double, Double, Vector[GeneratorFuel]](
      "ClassName",
      "mDisplayName",
      "mPowerProduction",
      "mPowerConsumptionExponent",
      "mSupplementalToPowerRatio",
      "mFuel"
    )( PowerGenerator( _, _, _, _, _, _ ) )

}
