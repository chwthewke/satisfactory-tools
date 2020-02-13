package net.chwthewke.satisfactorytools.model

import cats.syntax.functor._
import io.circe.Decoder
import mouse.boolean._
import scala.concurrent.duration._

final case class Extractor(
    className: ClassName,
    displayName: String,
    allowedResourceForms: List[Form],
    allowedResources: Option[List[ClassName]],
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
      ) => Extractor( cn, dn, arf, fr.option( rf ), pc, ct.seconds, ic )
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

}
