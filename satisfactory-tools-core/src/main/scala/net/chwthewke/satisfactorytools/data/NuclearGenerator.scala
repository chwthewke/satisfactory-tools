package net.chwthewke.satisfactorytools
package data

import cats.Show
import cats.derived.semiauto
import io.circe.Decoder

case class NuclearGenerator(
    className: ClassName,
    displayName: String,
    powerProduction: Double,
    fuels: Map[ClassName, Countable[Int, ClassName]]
)

object NuclearGenerator {
  implicit val nuclearGeneratorShow: Show[NuclearGenerator] = semiauto.show[NuclearGenerator]

  private implicit val fuelsDecoder: Decoder[Map[ClassName, Countable[Int, ClassName]]] =
    Decoder
      .decodeVector[( ClassName, Countable[Int, ClassName] )](
        Decoder.forProduct3[( ClassName, Countable[Int, ClassName] ), ClassName, ClassName, Int](
          "mFuelClass",
          "mByproduct",
          "mByproductAmount"
        )( ( s, p, a ) => ( s, Countable( p, a ) ) )
      )
      .map( _.toMap )

  implicit val nuclearGeneratorDecoder: Decoder[NuclearGenerator] =
    Decoder.forProduct4[NuclearGenerator, ClassName, String, Double, Map[ClassName, Countable[Int, ClassName]]](
      "ClassName",
      "mDisplayName",
      "mPowerProduction",
      "mFuel"
    )( NuclearGenerator( _, _, _, _ ) )

}
