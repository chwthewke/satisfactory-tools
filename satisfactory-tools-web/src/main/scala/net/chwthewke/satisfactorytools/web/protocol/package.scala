package net.chwthewke.satisfactorytools
package web

import cats.syntax.either._
import cats.syntax.option._
import enumeratum.Enum
import enumeratum.EnumEntry
import scodec.Codec
import scodec.bits.Bases.Alphabets
import scodec.bits.BitVector
import scodec.codecs

package object protocol {

  def enumCodec[E <: EnumEntry]( implicit E: Enum[E] ): Codec[E] = {
    val size = (32 - java.lang.Integer.numberOfLeadingZeros( E.values.size - 1 )).toLong

    def enumToBits( e: E ): BitVector    = BitVector( E.valuesToIndex( e ) ).takeRight( size )
    def enumOfBits( bits: BitVector ): E = E.values( bits.toInt( signed = false ) )

    codecs.bits( size ).xmap( enumOfBits, enumToBits )
  }

  def enumSetCodec[E <: EnumEntry]( implicit E: Enum[E] ): Codec[Set[E]] =
    subsetCodec[Set]( E.values )

  def subsetCodec[CC[x] <: Iterable[x]] = new SubsetCodecPartiallyApplied[CC]

  def toBase64[A]( codec: Codec[A] )( a: A ): String =
    codec.encode( a ).map( _.toBase64( Alphabets.Base64Url ) ).toOption.orEmpty

  def fromBase64[A]( codec: Codec[A] )( base64: String ): Either[String, A] =
    BitVector
      .fromBase64Descriptive( base64, Alphabets.Base64Url )
      .flatMap( codec.decode( _ ).toEither.leftMap( _.message ).map( _.value ) )

}
