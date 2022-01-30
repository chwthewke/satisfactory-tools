package net.chwthewke.dsptools
package gamedata

import cats.Show
import cats.syntax.apply._
import cats.syntax.show._
import scodec.Decoder
import scodec.codecs._
import scodec.interop.cats._

case class StringProto(
    name: String,
    id: Int,
    sId: String,
    zhCN: String,
    enUS: String,
    frFR: String
)

object StringProto extends Decoders {
  implicit val decoder: Decoder[StringProto] =
    (
      alignedUtf8.withLog( "string name" ),
      int32L.withLog( "string id" ),
      alignedUtf8.withLog( "sId" ),
      alignedUtf8.withLog( "zh_cn" ),
      alignedUtf8.withLog( "en_us" ),
      alignedUtf8.withLog( "fr_fr" )
    ).mapN( StringProto( _, _, _, _, _, _ ) )
      .withToString( "StringProto" )

  implicit val stringProtoShow: Show[StringProto] = Show.show {
    case StringProto( name, id, sId, zhCN, enUS, frFR ) =>
      show"""ID:    $id
            |Name:  $name
            |sID:   $sId
            |zh_CN: $zhCN
            |en_US: $enUS
            |fr_FR: $frFR
            |""".stripMargin
  }

}
