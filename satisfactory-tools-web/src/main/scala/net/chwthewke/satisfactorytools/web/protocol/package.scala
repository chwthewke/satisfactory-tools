package net.chwthewke.satisfactorytools
package web

import cats.data.Chain
import cats.data.Validated.Valid
import cats.data.ValidatedNel
import cats.syntax.either._
import cats.syntax.traverse._
import enumeratum.Enum
import enumeratum.EnumEntry
import org.http4s.FormDataDecoder
import org.http4s.ParseFailure
import scodec.Codec
import scodec.bits.Bases.Alphabets
import scodec.bits.BitVector
import scodec.codecs

package object protocol {

  ///////////////////////
  // CODECS

  def enumCodec[E <: EnumEntry]( implicit E: Enum[E] ): Codec[E] = {
    val size = (32 - java.lang.Integer.numberOfLeadingZeros( E.values.size - 1 )).toLong

    def enumToBits( e: E ): BitVector    = BitVector( E.valuesToIndex( e ) ).takeRight( size )
    def enumOfBits( bits: BitVector ): E = E.values( bits.toInt( signed = false ) )

    codecs.bits( size ).xmap( enumOfBits, enumToBits )
  }

  def enumSetCodec[E <: EnumEntry]( implicit E: Enum[E] ): Codec[Set[E]] =
    subsetCodec[Set]( E.values )

  def subsetCodec[CC[x] <: Iterable[x]] = new SubsetCodecPartiallyApplied[CC]

  // BASE 64

  def toBase64[A]( codec: Codec[A] )( a: A ): Either[String, String] =
    codec.encode( a ).toEither.leftMap( _.message ).map( _.toBase64( Alphabets.Base64Url ) )

  def fromBase64[A]( codec: Codec[A] )( base64: String ): Either[String, A] =
    BitVector
      .fromBase64Descriptive( base64, Alphabets.Base64Url )
      .flatMap( codec.decode( _ ).toEither.leftMap( _.message ).map( _.value ) )

  ////////////////////
  // FORM DATA

  private def validateEnum[E <: EnumEntry]( v: String )( implicit E: Enum[E] ): ValidatedNel[ParseFailure, E] =
    E.withNameEither( v ).leftMap( ex => ParseFailure( "", ex.getMessage ) ).toValidatedNel

  def enumFormDataDecoder[E <: EnumEntry]( key: String )( implicit E: Enum[E] ): FormDataDecoder[E] =
    FormDataDecoder
      .field[String]( key )
      .mapValidated( validateEnum[E] )

  def enumSetFormDataDecoder[E <: EnumEntry]( key: String )( implicit E: Enum[E] ): FormDataDecoder[Set[E]] =
    FormDataDecoder( fd => Valid( fd.getOrElse( key, Chain.empty ) ) )
      .mapValidated( strs => strs.traverse( validateEnum[E] ).map( _.toVector.toSet ) )

}
