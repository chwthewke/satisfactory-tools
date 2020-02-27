package net.chwthewke.satisfactorytools
package model

import cats.Show
import cats.data.NonEmptyList
import cats.instances.int._
import cats.instances.finiteDuration._
import cats.instances.list._
import cats.instances.string._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.show._
import io.circe.Decoder
import mouse.boolean._
import mouse.option._
import scala.concurrent.duration._

final case class Extractor(
    className: ClassName,
    displayName: String,
    allowedResourceForms: List[Form],
    allowedResources: Option[NonEmptyList[ClassName]],
    powerConsumption: Double,
    cycleTime: FiniteDuration,
    itemsPerCycle: Int
)
object Extractor {

  implicit val extractorDecoder: Decoder[Extractor] = {
    import Parsers._

    Decoder.forProduct8(
      "ClassName",
      "mDisplayName",
      "mAllowedResourceForms",
      "mOnlyAllowCertainResources",
      "mAllowedResources",
      "mPowerConsumption",
      "mExtractCycleTime",
      "mItemsPerCycle"
    )(
      (
          cn: ClassName,
          dn: String,
          arf: List[Form],
          fr: Boolean,
          rf: List[ClassName],
          pc: Double,
          ct: Double,
          ic: Int
      ) => Extractor( cn, dn, arf, NonEmptyList.fromList( rf ).flatMap( fr.option( _ ) ), pc, ct.seconds, ic )
    )(
      Decoder[ClassName],
      Decoder[String],
      listOf( `enum`( Form ) ).decoder,
      Decoders.booleanStringDecoder,
      listOf( bpGeneratedClass ).decoder
        .or( Decoder[String].ensure( _.isEmpty, "Cannot decode allowed resources" ).as( List.empty[ClassName] ) ),
      Decoders.doubleStringDecoder,
      Decoders.doubleStringDecoder,
      Decoders.intStringDecoder
    )
  }

  val extractorClass: ClassName = ClassName( "Build_Converter_C" )

  implicit val extractorShow: Show[Extractor] = Show { extractor =>
    show"""${extractor.displayName} # ${extractor.className}
          |${extractor.itemsPerCycle} / ${extractor.cycleTime}
          |Power: ${f"${extractor.powerConsumption}%.0f"} MW
          |Resource forms: ${extractor.allowedResourceForms.map( _.show ).intercalate( ", " )}
          |Resources: ${extractor.allowedResources.cata( _.toList.map( _.show ).intercalate( ", " ), "any" )}
          |""".stripMargin
  }

}
