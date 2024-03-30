package net.chwthewke.satisfactorytools

import cats.data.NonEmptyVector
import cats.effect.Async
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.std.Console
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.order._
import cats.syntax.traverse._
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

class IndexIcons[F[_]]( implicit S: Async[F], F: Files[F] ) {

  private val console: Console[F] = Console.make[F]

  private def readIconFiles( version: DataVersionStorage ): Stream[F, ( String, Path, ByteVector )] = {
    val iconsDir: Path = GrabIcons.targetDirectory / version.docsKey
    Stream
      .eval( F.isDirectory( iconsDir ) )
      .ifM( iconFilesIn( iconsDir ), Stream.empty )
      .map { case ( n, p, h ) => ( n, GrabIcons.targetDirectory.relativize( p ), h ) }
  }

  private def readAllIconFiles: F[Map[( String, DataVersionStorage ), ( Path, ByteVector )]] =
    Stream
      .emits( DataVersionStorage.values )
      .flatMap( v => readIconFiles( v ).tupleLeft( v ) )
      .map { case ( v, ( n, p, h ) ) => ( ( n, v ), ( p, h ) ) }
      .compile
      .to( Map )

  private def loadTextureNames: F[Map[String, NonEmptyVector[DataVersionStorage]]] =
    DataVersionStorage.values
      .flatTraverse(
        v =>
          Loader[F]
            .loadGameData( v )
            .map( _.items.values.toVector.map( _._1.smallIcon.textureName ).distinct.tupleLeft( v ) )
      )
      .map( _.foldMap { case ( v, n ) => Map( ( n, NonEmptyVector.one( v ) ) ) } )

  private def textureName256to64( name: String ): Option[String] =
    Option.when( name.endsWith( "_256" ) )( s"${name.stripSuffix( "_256" )}_64" )

  private def findIcon(
      files: Map[( String, DataVersionStorage ), ( Path, ByteVector )],
      name: String,
      version: DataVersionStorage,
      expectedHash: Option[ByteVector]
  ): Option[( Path, ByteVector )] = {

    val res = files
      .get( ( name, version ) )
      .orElse( textureName256to64( name ).flatMap( name64 => files.get( ( name64, version ) ) ) )
      .filter { case ( _, hash ) => expectedHash.forall( _ == hash ) }

    println( s"GET $name $version ->> $res" )

    res
  }

  private def computeIconIndex(
      files: Map[( String, DataVersionStorage ), ( Path, ByteVector )],
      textures: Map[String, NonEmptyVector[DataVersionStorage]]
  ): Map[( String, ModelVersion ), Path] =
    textures.flatMap {
      case ( name, versions ) =>
        versions.toVector.flatMap { version =>
          println( s"searching for texture $name for version $version" )
          // 1. find most likely version with this name
          (DataVersionStorage.values.filter( _ > version )
            ++ DataVersionStorage.values.filter( _ <= version ).reverse)
            .collectFirstSome { earlierVersion =>
              findIcon( files, name, earlierVersion, None ).tupleRight( earlierVersion )
            }
            .flatMap {
              case ( ( path, hash ), candidateVersion ) =>
                println( s"candidate found $path in version $candidateVersion" )
                // 2. find most recent version with same hash
                DataVersionStorage.values
                  .filter( _ >= candidateVersion )
                  .reverse
                  .collectFirstSome( laterVersion => findIcon( files, name, laterVersion, Some( hash ) ) )
                  .map {
                    case ( path, _ ) =>
                      println( s"candidate replaced with same-hash $path" )
                      path
                  }
            }
            .tupleLeft( ( name, version.modelVersion ) )
        }
    }

  private def computeUnusedFiles(
      files: Map[( String, DataVersionStorage ), ( Path, ByteVector )],
      index: Map[( String, ModelVersion ), Path]
  ): Vector[Path] = {
    val usedFiles: Set[Path] = index.values.toSet
    files.map( _._2._1 ).filterNot( usedFiles ).toVector
  }

  def createIndex: F[IconIndex] =
    for {
      textures <- loadTextureNames
      files    <- readAllIconFiles
      _        <- Console.make[F].println( files.toVector.mkString( "FILES\n  ", "\n  ", "" ) )
      index       = computeIconIndex( files, textures )
      unusedFiles = computeUnusedFiles( files, index )
      _ <- unusedFiles.traverse_( /*F.delete*/ f => Console.make[F].println( s"delete $f" ) )
    } yield IconIndex(
      index.map {
        case ( ( name, version ), path ) =>
          (
            ( name, version.version ),
            path.toString.replace( '\\', '/' )
          )
      }
    )

  private def computeHash( file: Path ): F[ByteVector] =
    F.readAll( file ).through( hash.sha256[F] ).compile.to( ByteVector )

  private def iconFilesIn( dir: Path ): Stream[F, ( String, Path, ByteVector )] =
    F.list( dir )
      .evalFilter( f => F.isRegularFile( f ) )
      .filter( _.fileName.toString.endsWith( ".png" ) )
      .evalMap( f => computeHash( f ).map( ( f.fileName.toString.stripSuffix( ".png" ), f, _ ) ) )

  private def writeIndex( iconIndex: IconIndex ): F[Unit] =
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
  val output: Path = Path( "assets" ) / "src" / "main" / "resources" / "icons.conf"

  override def run( args: List[String] ): IO[ExitCode] =
    new IndexIcons[IO].run.as( ExitCode.Success )
}
