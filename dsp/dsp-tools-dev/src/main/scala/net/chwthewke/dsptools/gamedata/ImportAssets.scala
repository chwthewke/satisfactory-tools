package net.chwthewke.dsptools
package gamedata

import cats.data.EitherT
import cats.data.OptionT
import cats.data.StateT
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.Resource
import cats.effect.Sync
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import fs2.Pull
import fs2.Stream
import fs2.interop.scodec.CodecError
import fs2.io.file.FileHandle
import fs2.io.file.Files
import fs2.io.file.Flags
import fs2.io.file.Path
import fs2.io.file.ReadCursor
import scodec.Attempt
import scodec.DecodeResult
import scodec.Decoder
import scodec.Err
import scodec.bits.BitVector

/**
  * Utilities for parsing Unity files and getting our stuff
  */
abstract class ImportAssets[F[_]: Sync]( implicit files: Files[F] ) extends UnityDecoders17 {

  def openFile( path: Path ): Resource[F, FileHandle[F]] = files.open( path, Flags.Read )

  def decode_[A]( readCursor: ReadCursor[F], chunkSize: Int = 512 )(
      decoder: Decoder[A]
  ): F[( ReadCursor[F], A )] = {
    ( readCursor, BitVector.empty, chunkSize )
      .tailRecM[EitherT[F, Err, *], ( ReadCursor[F], A )] {
        case ( curs, buf, sz ) =>
          OptionT( curs.read( sz ) )
            .toRight( Err.General( s"Failed to read from $readCursor", Nil ) )
            .flatMap {
              case ( nextCurs, bytes ) =>
                val nextBuf = buf ++ bytes.toBitVector
                decoder.decode( nextBuf ) match {
                  case Attempt.Successful( DecodeResult( _, remainder ) ) if remainder.size % 8 != 0L =>
                    EitherT.leftT( Err.General( s"Decoder $decoder consumed fractional byte.", Nil ) )
                  case Attempt.Successful( DecodeResult( value, remainder ) ) =>
                    EitherT.rightT( Right( ( nextCurs.seek( nextCurs.offset - (remainder.size / 8L) ), value ) ) )
                  case Attempt.Failure( _: Err.InsufficientBits ) =>
                    EitherT.rightT( Left( ( nextCurs, nextBuf, sz * 2 ) ) )
                  case Attempt.Failure( comp: Err.Composite )
                      if comp.errs.exists( _.isInstanceOf[Err.InsufficientBits] ) =>
                    EitherT.rightT( Left( ( nextCurs, nextBuf, sz * 2 ) ) )
                  case Attempt.Failure( cause ) =>
                    EitherT.leftT( cause )
                }
            }
      }
      .leftMap( CodecError )
      .rethrowT
  }

  def decode[A]( decoder: Decoder[A], sizeHint: Int = 512 ): StateT[F, ReadCursor[F], A] =
    StateT( cursor => decode_( cursor, sizeHint )( decoder ) )

  def decodeSerializedTypes(
      fileEndianness: Endianness,
      hasTypeTree: Boolean
  ): StateT[F, ReadCursor[F], Vector[SerializedType]] =
    for {
      count <- decode( fileEndianness.int32.withToString( "type count" ).withLog( "" ), 16 )
      types <- decode( serializedType( fileEndianness, hasTypeTree ), 40 ).replicateA( count )
    } yield types.toVector

  def pos: StateT[F, ReadCursor[F], Long]                  = StateT.inspect( _.offset )
  def seek( offset: Long ): StateT[F, ReadCursor[F], Unit] = StateT.modify( _.seek( offset ) )

  def decodeSerializedObjects(
      fileEndianness: Endianness,
      dataOffset: Long,
      types: Vector[SerializedType]
  ): StateT[F, ReadCursor[F], Vector[ObjectInfo]] =
    for {
      count  <- decode( fileEndianness.int32.withToString( "object count" ).withLog( "" ), 4 )
      offset <- pos
      objects <- decode( serializedObject( fileEndianness, offset, types ), 32 )
                  .map( obj => obj.copy( byteStart = obj.byteStart + dataOffset ) )
                  .replicateA( count )
    } yield objects.toVector

  def liftF[A]( fa: F[A] ): StateT[F, ReadCursor[F], A] = StateT.liftF( fa )

  def readObjectName(
      hnd: FileHandle[F],
      fileEndianness: Endianness,
      objectInfo: ObjectInfo
  ): F[String] = {
    val offset = objectInfo.byteStart + 25L
    decode( objectName( fileEndianness ), 512 )
      .runA( ReadCursor( hnd, offset + skipToAlign( offset ) ) )
  }

  private def streamFrom( readCursor: ReadCursor[F] ): Stream[F, Byte] = {
    def go( c: ReadCursor[F] ): Pull[F, Byte, Unit] = c.readPull( 512 ).flatMap {
      case Some( ( curs, chunk ) ) =>
        println( s"Got chunk size ${chunk.size}" )
        Pull.output( chunk ) >> go( curs )
      case None =>
        println( "End of file" )
        Pull.pure( () )
    }
    go( readCursor ).stream
  }

  def exportRawObject(
      hnd: FileHandle[F],
      fileEndianness: Endianness,
      objectInfo: ObjectInfo,
      dest: Path,
      filter: String => Boolean
  ): F[Unit] =
    readObjectName( hnd, fileEndianness, objectInfo )
      .flatMap(
        name =>
          files
            .writeAll( dest / s"$name.dat" )(
              streamFrom( ReadCursor( hnd, objectInfo.byteStart ) ).take( objectInfo.byteSize )
            )
            .compile
            .drain
            .whenA( filter( name ) )
      )
      .whenA( objectInfo.classId == MonoBehaviourClassId )

}

object ImportAssets extends IOApp {

  val base: Path = Path( "D:\\SteamLibrary\\steamapps\\common\\Dyson Sphere Program\\DSPGAME_Data" )
  val dest: Path = Path( "dsp\\dsp-tools-core\\src\\main\\resources" ).absolute

  override def run( args: List[String] ): IO[ExitCode] =
    io.readDspProtoSets( base, dest ).as( ExitCode.Success )

  object io extends ImportAssets[IO] {

    val requiredData: Set[String] = Set( "ItemProtoSet", "RecipeProtoSet", "StringProtoSet", "TechProtoSet" )

    val resourceFile: String = "resources.assets"

    def readDspProtoSets( source: Path, dest: Path ): IO[Unit] =
      openFile( source / resourceFile )
        .use { hnd =>
          val p = for {
            actualSize <- liftF( hnd.size )
            fileHeader <- decode( serializedFileHeader, 64 )
            _ <- liftF( IO.raiseError( new RuntimeException( s"Not a serialized file ${source / resourceFile}" ) ) )
                  .whenA( !fileHeader.isValid( actualSize ) )
            types   <- decodeSerializedTypes( fileHeader.endianness, fileHeader.hasTypeTree )
            objects <- decodeSerializedObjects( fileHeader.endianness, fileHeader.dataOffset, types )
            _ <- liftF(
                  objects.traverse_( exportRawObject( hnd, fileHeader.endianness, _, dest, requiredData ) )
                )
          } yield ()

          p.runA( ReadCursor( hnd, 0L ) )
        }
  }

}
