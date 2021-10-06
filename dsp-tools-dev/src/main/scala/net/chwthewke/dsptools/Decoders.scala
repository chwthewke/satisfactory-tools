package net.chwthewke.dsptools

import cats.Show
import cats.syntax.apply._
import cats.syntax.show._
import enumeratum.Enum
import enumeratum.EnumEntry
import scala.language.implicitConversions
import scodec.Attempt
import scodec.DecodeResult
import scodec.Decoder
import scodec.Err
import scodec.bits.BitVector
import scodec.bits.ByteVector
import scodec.codecs._
import scodec.interop.cats._

trait Decoders {

  implicit class DecoderOps[A]( private val self: Decoder[A] ) {
    def withLog( mag: LogMagnet[A] ): Decoder[A] =
      new Decoder[A] {
        override def toString: String = self.toString

        override def decode( bits: BitVector ): Attempt[DecodeResult[A]] =
          self.decode( bits ).map {
            case res @ DecodeResult( value, remainder ) =>
              println( s"${self.toString} OK ${mag( value )}" )
              println(
                s"remaining: ${remainder.size / 8} bytes"
                  .padTo( 40, ' ' ) + remainder.toByteVector.take( 16L ).toSeq.map( d => f"$d%02X" ).mkString( " " )
              )
              res
          }
      }

    def withToString( str: String ): Decoder[A] =
      new Decoder[A] {
        override def decode( bits: BitVector ): Attempt[DecodeResult[A]] = self.decode( bits )

        override def toString: String = str
      }
  }

  trait LogMagnet[A] {
    def apply( value: A ): String
  }

  object LogMagnet extends LogMagnetLow {
    implicit def logMagnet( desc: String ): LogMagnet[Unit] = _ => desc
  }

  trait LogMagnetLow {
    implicit def logMagnet[A: Show]( desc: String ): LogMagnet[A] = (value: A) => show"$desc $value"
  }

  val Header: BitVector = (ByteVector.fill( 12L )( 0 ) ++ ByteVector( 1, 0, 0, 0 )).toBitVector

  val headerDecoder: Decoder[Unit] = (constant( Header ) <~ ignore( 12L * 8L )).asDecoder
    .withLog( "<HEADER>" )
    .withToString( "ProtoSet header" )

  def ignorePad( length: Int ): Decoder[Unit] =
    ignore( length.toLong * 8L ).withLog( _ => s"$length bytes pad ignored" )

  val alignedUtf8: Decoder[String] =
    int32L
      .withLog( "String size" )
      .flatMap( sz => fixedSizeBytes( sz.toLong, utf8 ).asDecoder <* ignorePad( 3 - (sz + 3) % 4 ) )
      .withToString( "aligned UTF-8 string" )

  def decodeEnum[A <: EnumEntry]( implicit A: Enum[A] ): Decoder[A] =
    int32L
      .emap( n => Attempt.fromOption( A.values.lift( n ), Err( s"Invalid index $n for enum $A" ) ) )
      .withToString( s"enum $A from $int32L" )

  def decodeVector[P]( decodeItem: Decoder[P] ): Decoder[Vector[P]] =
    int32L
      .withLog( n => s"$n items in array" )
      .flatMap( sz => Decoder.decodeCollect[Vector, P]( decodeItem, Some( sz ) ) )
      .withToString( s"array of $decodeItem" )

  val dspBool: Decoder[Boolean] = int32L.map( _ != 0 ).withToString( "boolean" )

}
