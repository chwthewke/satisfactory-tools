package net.chwthewke.dsptools
package gamedata

import cats.Show
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.show._
import scodec.Decoder

import loader.Loader

object TestGameData extends IOApp {
  override def run( args: List[String] ): IO[ExitCode] =
    testDecodeRecipeProtoSet.as( ExitCode.Success )

  def testDecodeStringProtoSet: IO[ProtoSet[StringProto]] =
    testDecodeProtoSet( "StringProtoSet.dat" )

  def testDecodeItemProtoSet: IO[ProtoSet[ItemProto]] =
    testDecodeProtoSet( "ItemProtoSet.dat" )

  def testDecodeRecipeProtoSet: IO[ProtoSet[RecipeProto]] =
    testDecodeProtoSet( "RecipeProtoSet.dat" )

  def testDecodeTechProtoSet: IO[ProtoSet[TechProto]] =
    testDecodeProtoSet( "TechProtoSet.dat" )

  def testDecodeProtoSet[P: Show]( resource: String )( implicit decoder: Decoder[P] ): IO[ProtoSet[P]] =
    Loader.io
      .loadProtoSetResource[P]( resource )
      .flatTap( result => IO.println( s"Success decoding ${result.protos.size} ${result.tableName}(s)" ) )
      .flatTap( result => IO.println( result.show ) )

}
