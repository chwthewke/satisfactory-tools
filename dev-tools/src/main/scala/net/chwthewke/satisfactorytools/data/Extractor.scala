package net.chwthewke.satisfactorytools
package data

import cats.Show
import cats.data.NonEmptyList
import cats.syntax.all._
import io.circe.Decoder
import mouse.boolean._
import mouse.option._
import scala.concurrent.duration._

final case class Extractor(
    className: ClassName,
    displayName: String,
    extractorTypeName: String,
    allowedResourceForms: List[Form],
    allowedResources: Option[NonEmptyList[ClassName]],
    powerConsumption: Double,
    powerConsumptionExponent: Double,
    cycleTime: FiniteDuration,
    itemsPerCycle: Int
)

object Extractor {

  implicit val extractorDecoder: Decoder[Extractor] = {
    import Parsers._

    Decoder.forProduct10(
      "ClassName",
      "mDisplayName",
      "mExtractorTypeName",
      "mAllowedResourceForms",
      "mOnlyAllowCertainResources",
      "mAllowedResources",
      "mPowerConsumption",
      "mPowerConsumptionExponent",
      "mExtractCycleTime",
      "mItemsPerCycle"
    )(
      (
          cn: ClassName,
          dn: String,
          etn: String,
          arf: List[Form],
          fr: Boolean,
          rf: List[ClassName],
          pc: Double,
          pe: Double,
          ct: Double,
          ic: Int
      ) => Extractor( cn, dn, etn, arf, NonEmptyList.fromList( rf ).flatMap( fr.option( _ ) ), pc, pe, ct.seconds, ic )
    )(
      Decoder[ClassName],
      Decoder[String],
      Decoder[String],
      listOf( `enum`( Form ) ).decoder,
      Decoders.booleanStringDecoder,
      listOf( bpGeneratedClass ).decoder
        .or( Decoder[String].ensure( _.isEmpty, "Cannot decode allowed resources" ).as( List.empty[ClassName] ) ),
      Decoders.doubleStringDecoder,
      Decoders.doubleStringDecoder,
      Decoders.doubleStringDecoder,
      Decoders.intStringDecoder
    )
  }

  val converterClass: ClassName = ClassName( "Build_Converter_C" )

  val waterExtractorClass: ClassName    = ClassName( "Build_WaterPump_C" )
  val oilExtractorClass: ClassName      = ClassName( "Build_OilPump_C" )
  val frackingExtractorClass: ClassName = ClassName( "Build_FrackingExtractor_C" )

  implicit val extractorShow: Show[Extractor] = Show { extractor =>
    show"""${extractor.displayName} # ${extractor.className}
          |${extractor.itemsPerCycle} / ${extractor.cycleTime}
          |Power: ${f"${extractor.powerConsumption}%.0f"} MW (exp: ${f"${extractor.powerConsumptionExponent}%.4f"})
          |Resource forms: ${extractor.allowedResourceForms.map( _.show ).intercalate( ", " )}
          |Resources: ${extractor.allowedResources.cata( _.toList.map( _.show ).intercalate( ", " ), "any" )}
          |""".stripMargin
  }

}
