package net.chwthewke.dsptools

import cats.Show
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.applicativeError._
import cats.syntax.either._
import cats.syntax.show._
import fs2.io.file.Files
import fs2.io.file.Path
import scodec.Decoder
import scodec.bits.ByteVector

object Test extends IOApp {
  override def run( args: List[String] ): IO[ExitCode] =
    testDecodeRecipeProtoSet.as( ExitCode.Success )

  def testDecodeStringProtoSet: IO[ProtoSet[StringProto]] =
    testDecodeProtoSet(
      Path( "C:\\Users\\Chewie\\Desktop\\dsp\\StringProtoSet.dat" )
    )

  def testDecodeItemProtoSet: IO[ProtoSet[ItemProto]] =
    testDecodeProtoSet(
      Path( "C:\\Users\\Chewie\\Desktop\\dsp\\ItemProtoSet.dat" )
    )

  def testDecodeRecipeProtoSet: IO[ProtoSet[RecipeProto]] =
    testDecodeProtoSet(
      Path( "C:\\Users\\Chewie\\Desktop\\dsp\\RecipeProtoSet.dat" )
    )

  def testDecodeTechProtoSet: IO[ProtoSet[TechProto]] =
    testDecodeProtoSet(
      Path( "C:\\Users\\Chewie\\Desktop\\dsp\\TechProtoSet.dat" )
    )

  def testDecodeProtoSet[P: Show]( path: Path )( implicit decoder: Decoder[P] ): IO[ProtoSet[P]] =
    Files[IO]
      .readAll( path )
      .compile
      .to( ByteVector )
      .attemptT
      .subflatMap(
        bytes =>
          ProtoSet
            .decoder( decoder )
            .complete
            .decode( bytes.toBitVector )
            .toEither
            .leftMap( err => new IllegalArgumentException( err.messageWithContext.take( 32768 ) ) )
            .map( _.value )
      )
      .semiflatTap(
        result => IO.println( s"Success decoding ${result.protos.size} ${result.tableName}(s)" )
      )
      .semiflatTap(
        result => IO.println( result.show )
      )
      .rethrowT
}
