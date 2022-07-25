package net.chwthewke.satisfactorytools

import cats.data.OptionT
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.Sync
import cats.effect.std.Console
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.order._
import com.typesafe.config.ConfigRenderOptions
import fs2.Stream
import fs2.hash
import fs2.io.file.Files
import fs2.io.file.Path
import pureconfig.module.fs2.saveConfigToStream
import scodec.bits.ByteVector

import model.ModelVersion
import assets.IconIndex
import loader.Loader

class IndexIcons[F[_]]( implicit S: Sync[F], F: Files[F] ) {

  private val console: Console[F] = Console.make[F]

  def computeHash( file: Path ): F[ByteVector] =
    F.readAll( file ).through( hash.sha256[F] ).compile.to( ByteVector )

  def iconFilesIn( dir: Path ): Stream[F, ( String, Path, ByteVector )] =
    F.list( dir )
      .evalFilter( f => F.isRegularFile( f ) )
      .filter( _.fileName.toString.endsWith( ".png" ) )
      .evalMap( f => computeHash( f ).map( ( f.fileName.toString.stripSuffix( ".png" ), f, _ ) ) )

  def addToIndex(
      preIndex: Map[( String, ModelVersion ), ( Path, ByteVector )],
      version: DataVersionStorage
  ): F[Map[( String, ModelVersion ), ( Path, ByteVector )]] = {
    val iconsDir = GrabIcons.targetDirectory / version.docsKey

    val readIconFiles: F[Map[String, ( Path, ByteVector )]] =
      console.println( s"List files in $iconsDir" ) *>
        Stream
          .eval( F.isDirectory( iconsDir ) )
          .ifM( iconFilesIn( iconsDir ), Stream.empty )
          .map { case ( n, p, h ) => ( n, ( GrabIcons.targetDirectory.relativize( p ), h ) ) }
          .evalTap { case ( _, ( p, h ) ) => console.println( s"$p (${h.toHex})" ) }
          .compile
          .foldChunks( Map.empty[String, ( Path, ByteVector )] )( ( m, c ) => m ++ c.iterator )

    ( Loader[F].loadGameData( version ), readIconFiles ).flatMapN { ( data, iconFiles ) =>
      val textureNames: Vector[String] = data.items.values.toVector.map( _.smallIcon.textureName ).distinct

      textureNames.foldLeftM( preIndex ) { ( acc, textureName ) =>
        val currentVersion: Option[( Path, ByteVector )] =
          iconFiles.get( textureName )

        val nextVersion: Option[( Path, ByteVector )] =
          DataVersionStorage.values
            .map( _.modelVersion )
            .filter( _ > version.modelVersion )
            .collectFirstSome( v => acc.get( ( textureName, v ) ) )

        val selected: OptionT[F, ( Path, ByteVector )] = OptionT(
          ( currentVersion, nextVersion ).traverseN {
            case ( ( currPath, currHash ), ( nextPath, nextHash ) ) =>
              if (currHash == nextHash)
                console.println( s"$currPath replaced with identical $nextPath (${nextHash.toHex})" ) *>
                  F.delete( GrabIcons.targetDirectory.resolve( currPath ) ).as( ( nextPath, nextHash ) )
              else
                console
                  .println( s"$currPath differs from $nextPath (${currHash.toHex} / ${nextHash.toHex})" )
                  .as( ( currPath, currHash ) )
          }
        ).orElse( OptionT.fromOption( currentVersion.orElse( nextVersion ) ) )

        selected.fold( acc )( ph => acc + ( ( ( textureName, version.modelVersion ), ph ) ) )
      }

    }
  }

  def createIndex: F[IconIndex] =
    DataVersionStorage.values.reverse
      .foldLeftM( Map.empty[( String, ModelVersion ), ( Path, ByteVector )] )( addToIndex )
      .map(
        m =>
          IconIndex( m.map {
            case ( ( n, v ), ( p, _ ) ) =>
              ( ( n, v.version ), p.toString.replace( '\\', '/' ) )
          } )
      )

  def writeIndex( iconIndex: IconIndex ): F[Unit] =
    saveConfigToStream[F, IconIndex]( iconIndex, ConfigRenderOptions.defaults().setOriginComments( false ) )
      .through( F.writeAll( IndexIcons.output ) )
      .compile
      .drain

  def run: F[Unit] =
    createIndex
      .flatTap( console.println( _ ) )
      .flatMap( writeIndex )

}

object IndexIcons extends IOApp {
  val output: Path = Path( "satisfactory-tools-assets" ) / "src" / "main" / "resources" / "icons.conf"

  override def run( args: List[String] ): IO[ExitCode] =
    new IndexIcons[IO].run.as( ExitCode.Success )
}
