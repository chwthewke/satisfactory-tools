package net.chwthewke.dsptools
package gamedata

import cats.Show
import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.show._
import scodec.Decoder
import scodec.interop.cats._

case class ProtoSet[P](
    fileName: String,
    tableName: String,
    signature: String,
    protos: Vector[P]
)

object ProtoSet extends Decoders {
  implicit def decoder[P]( implicit itemDecoder: Decoder[P] ): Decoder[ProtoSet[P]] =
    headerDecoder *>
      (
        alignedUtf8.withLog( "file name" ),  // file name
        alignedUtf8.withLog( "table name" ), // table name
        alignedUtf8.withLog( "signature" ),  // signature
        decodeVector( itemDecoder )          // StringProto array
      ).mapN(
          ProtoSet( _, _, _, _ )
        )
        .withToString( s"ProtoSet[$itemDecoder]" )

  implicit def protoSetShow[P: Show]: Show[ProtoSet[P]] = Show.show {
    case ProtoSet( fileName, tableName, _, protos ) =>
      val sep = "=" * 64

      show"""################################################################
            |# ${fileName.padTo( 60, ' ' )} #
            |# ${show"${protos.size} ${tableName}s".padTo( 60, ' ' )} #
            |################################################################
            |""".stripMargin ++
        protos.mkString_( "", sep + "\n", sep )
  }
}
