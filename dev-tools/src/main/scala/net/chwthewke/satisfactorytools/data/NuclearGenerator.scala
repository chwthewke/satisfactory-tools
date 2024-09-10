package net.chwthewke.satisfactorytools
package data

import cats.Show
import cats.syntax.show._
import io.circe.Decoder

case class NuclearGenerator(
    className: ClassName,
    displayName: String,
    powerProduction: Double,
    powerConsumptionExponent: Double,
    fuels: Map[ClassName, Countable[Int, ClassName]]
)

object NuclearGenerator {
  implicit val nuclearGeneratorShow: Show[NuclearGenerator] = Show { generator =>
    show"""${generator.displayName} # ${generator.className}
          |Power: ${f"${generator.powerProduction}%.0f"} MW (exp: ${f"${generator.powerConsumptionExponent}%.4f"})
          |Fuels:
          |  ${generator.fuels.map { case ( f, b ) => show"$f -> $b" }.mkString( "\n  " )}
          |""".stripMargin

  }

  private case class ByProductAmount( val amount: Int ) extends AnyVal
  private object ByProductAmount {
    implicit val byProductAmountDecoder: Decoder[ByProductAmount] = Decoder
      .instance( hc =>
        hc.as[String] match {
          case Right( "" ) => Right( 0 )
          case _           => hc.as[Int]
        }
      )
      .map( ByProductAmount( _ ) )
  }

  private implicit val fuelsDecoder: Decoder[Map[ClassName, Countable[Int, ClassName]]] =
    Decoder
      .decodeVector[( ClassName, Countable[Int, ClassName] )] {
        Decoder.forProduct3[( ClassName, Countable[Int, ClassName] ), ClassName, ClassName, ByProductAmount](
          "mFuelClass",
          "mByproduct",
          "mByproductAmount"
        )( ( s, p, a ) => ( s, Countable( p, a.amount ) ) )
      }
      .map( _.toMap )

  implicit val nuclearGeneratorDecoder: Decoder[NuclearGenerator] =
    Decoder.forProduct5[NuclearGenerator, ClassName, String, Double, Double, Map[ClassName, Countable[Int, ClassName]]](
      "ClassName",
      "mDisplayName",
      "mPowerProduction",
      "mPowerConsumptionExponent",
      "mFuel"
    )( NuclearGenerator( _, _, _, _, _ ) )

}
