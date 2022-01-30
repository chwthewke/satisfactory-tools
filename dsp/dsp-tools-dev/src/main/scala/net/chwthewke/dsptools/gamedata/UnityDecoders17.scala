package net.chwthewke.dsptools
package gamedata

import cats.Show
import cats.derived.semiauto
import cats.syntax.applicative._
import scodec.Attempt
import scodec.DecodeResult
import scodec.Decoder
import scodec.Err
import scodec.bits.BitVector
import scodec.bits.ByteVector
import scodec.codecs._
import scodec.interop.cats._

trait UnityDecoders17 extends Decoders {

  val MonoBehaviourClassId: Int = 114

  sealed abstract class Endianness(
      val int16: Decoder[Int],
      val int32: Decoder[Int],
      val int64: Decoder[Long],
      val uint32: Decoder[Long],
      override val toString: String
  )

  object Endianness {

    object BigEndian    extends Endianness( int16, int32, int64, uint32, "Big-endian" )
    object LittleEndian extends Endianness( int16L, int32L, int64L, uint32L, "Little-endian" )

    implicit val endiannessShow: Show[Endianness] = Show.fromToString
  }

  def endianness: Decoder[Endianness] =
    byte.map( b => if (b == 0) Endianness.LittleEndian else Endianness.BigEndian ).withToString( "File endianness" )

  case class SerializedFileHeader(
      fileSize: Long,
      dataOffset: Long,
      endianness: Endianness,
      unityVersion: String,
      hasTypeTree: Boolean
  ) {
    def isValid( actualSize: Long ): Boolean =
      fileSize == actualSize && dataOffset <= fileSize
  }

  object SerializedFileHeader {
    implicit val serializedFileHeaderShow: Show[SerializedFileHeader] = semiauto.show[SerializedFileHeader]
  }

  val nullTerminatedUtf8: Decoder[String] = new Decoder[String] {
    override def decode( bits: BitVector ): Attempt[DecodeResult[String]] =
      bits.toByteVector.indexOfSlice( ByteVector( 0 ) ) match {
        case -1 => Attempt.failure( Err.InsufficientBits( -1L, bits.size, Nil ) )
        case n =>
          val bitsOfString = n * 8
          utf8.decode( bits.take( bitsOfString ) ).map( _.mapRemainder( _ => bits.drop( bitsOfString + 8 ) ) )
      }

    override def toString: String = "NULL-terminated UTF8 string"
  }

  val serializedFileHeader: Decoder[SerializedFileHeader] =
    for {
      _              <- ignore( 32 )
      fileSize       <- uint32.withLog( "SFHeader file size" )
      version        <- uint32.withLog( "SFHeader version" )
      _              <- if (version == 17L) provide( () ) else fail( Err( "Expected version 17" ) )
      dataOffset     <- uint32.withLog( "SFHeader data offset" )
      fileEndianness <- endianness.withLog( "endianness" )
      _              <- ignore( 24 )
      unityVersion   <- nullTerminatedUtf8.withLog( "Unity version" )
      buildTarget    <- fileEndianness.int32.withLog( "build target" )
      _              <- fail( Err( "Build target NoTarget not supported" ) ).asDecoder.whenA( buildTarget == -2 )
      hasTypeTree    <- bool( 8 ).withLog( "has type trees" )
    } yield SerializedFileHeader( fileSize, dataOffset, fileEndianness, unityVersion, hasTypeTree )

  case class SerializedType( classId: Int )

  def serializedType( E: Endianness, hasTypeTree: Boolean ): Decoder[SerializedType] =
    for {
      classId <- E.int32.withLog( "classID" )
      _       <- bool( 8 ).withLog( "is stripped" )
      _       <- E.int16.withLog( "script type index" )
      _       <- bytes( 16 ).withLog( "script ID" ).whenA( classId == MonoBehaviourClassId )
      _       <- ignore( 16 * 8 )
      _       <- fail( Err( "Type trees not supported" ) ).asDecoder.whenA( hasTypeTree )
    } yield SerializedType( classId )

  def serializedTypes( E: Endianness, hasTypeTree: Boolean ): Decoder[Vector[SerializedType]] =
    E.int32.withLog( "type count" ).flatMap( serializedType( E, hasTypeTree ).replicateA ).map( _.toVector )

  case class ObjectInfo(
      pathId: Long,
      byteStart: Long,
      byteSize: Long,
      classId: Int
  )

  def skipToAlign( offset: Long ): Long = (4 - offset % 4) % 4

  def align( offset: Long ): Decoder[Unit] = {
    val s = skipToAlign( offset )
    ignore( 8 * s ).withLog( s"skipped $s from $offset" )
  }

  def serializedObject(
      E: Endianness,
      offset: Long,
      types: Vector[SerializedType]
  ): Decoder[ObjectInfo] = {

    for {
      _         <- align( offset )
      pathId    <- E.int64.withLog( "path ID" )
      byteStart <- E.uint32.withLog( "byte start" )
      byteSize  <- E.uint32.withLog( "byte size" )
      typeId    <- E.uint32.withLog( "type index" )
      sType = types( typeId.toInt )
    } yield ObjectInfo( pathId, byteStart, byteSize, sType.classId )
  }

  def objectName( E: Endianness ): Decoder[String] =
    for {
      len <- E.int32.withLog( "name bytes" )
      res <- fixedSizeBytes( len.toLong, utf8 ).withLog( "object name" )
    } yield res

}
