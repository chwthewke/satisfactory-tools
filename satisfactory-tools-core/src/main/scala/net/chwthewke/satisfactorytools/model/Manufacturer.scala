package net.chwthewke.satisfactorytools.model

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

  val builders: Set[ClassName] = Set(
    "Build_ConstructorMk1_C",
    "Build_SmelterMk1_C",
    "Build_AssemblerMk1_C",
    "Build_Converter_C",
    "Build_OilRefinery_C",
    "Build_FoundryMk1_C",
    "Build_ManufacturerMk1_C"
  ).map( ClassName( _ ) )

}
