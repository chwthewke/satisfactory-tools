package net.chwthewke.satisfactorytools

import cats.effect.Async
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.std.Console
import cats.effect.syntax.monadCancel._
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import fs2.io.file.CopyFlag
import fs2.io.file.CopyFlags
import fs2.io.file.Files
import fs2.io.file.Path
import scala.collection.mutable
import scala.sys.process.ProcessLogger
import sys.process.Process

import data.GameData
import data.IconData
import loader.Loader

class GrabIcons[F[_]]( version: DataVersionStorage )( implicit S: Async[F], F: Files[F] ) {

  private val console: Console[F] = Console.make[F]

  private val target: Path = GrabIcons.targetDirectory / version.docsKey

  def run: F[Unit] =
    version.gameSource.fold( console.println( s"No icon source for ${version.entryName}" ) )(
      gs =>
        for {
          gameData <- new Loader[F].loadGameData( version )
          _        <- F.createDirectories( target )
          _        <- grabIcons( gs, gameData )
        } yield ()
    )

  def grabIcons( gameSource: DataVersionStorage.GameSource, gameData: GameData ): F[Unit] = {
    F.createTempDirectory.bracket(
      tmpDir =>
        gameData.items.values
          .map( item => item._1.smallIcon )
          .toVector
          .distinct
          .traverse_( grabIcon( gameSource, tmpDir, _ ) )
    )(
      F.deleteRecursively
    )
  }

  def grabIcon( gameSource: DataVersionStorage.GameSource, extractDir: Path, data: IconData ): F[Unit] =
    for {
      ( x, o, e ) <- runExtract( gameSource, extractDir, data )
      _           <- console.println( s"Extract texture ${data.textureName} returned $x" )
      _           <- console.errorln( s"Error while extracting ${data.textureName}\n$o\n$e" ).whenA( x != 0 )
      _           <- moveExtracted( extractDir, data )
    } yield ()

  def runExtract(
      gameSource: DataVersionStorage.GameSource,
      extractDir: Path,
      data: IconData
  ): F[( Int, String, String )] =
    S.delay {
      val out: mutable.StringBuilder = new mutable.StringBuilder()
      val err: mutable.StringBuilder = new mutable.StringBuilder()
      val logger                     = ProcessLogger( s => { out.append( s ); () }, s => { err.append( s ); () } )

      val exit: Int = Process(
        Seq(
          GrabIcons.pathToUModel,
          s"-game=${gameSource.ueVersionTag}",
          s"-path=${gameSource.path / "FactoryGame" / "Content" / "Paks"}",
          s"-out=$extractDir",
          "-png",
          "-export",
          data.packageName
        ),
        Some( extractDir.toNioPath.toFile )
      ).!( logger )

      ( exit, out.toString, err.toString )
    }

  def moveExtracted( extractDir: Path, data: IconData ): F[Unit] = {
    val textureFile: String = s"${data.textureName}.png"
    val source: Path        = extractDir.resolve( Path( data.dir ) ) / textureFile
    val dest: Path          = target / textureFile

    F.move( source, dest, CopyFlags( CopyFlag.ReplaceExisting ) )
      .handleErrorWith( e => console.errorln( s"Error while moving $source to $dest: ${e.getMessage}" ) )
  }

}

object GrabIcons {
  val pathToUModel: String  = "C:\\Users\\Chewie\\umodel_win32_202306\\umodel.exe"
  val targetDirectory: Path = Path( "assets" ) / "src" / "main" / "resources" / "img"

  abstract class Program( storage: DataVersionStorage ) extends IOApp {
    override def run( args: List[String] ): IO[ExitCode] =
      new GrabIcons[IO]( storage ).run.as( ExitCode.Success )
  }
}

object GrabIconsU4 extends GrabIcons.Program( DataVersionStorage.Update4 )
object GrabIconsU5 extends GrabIcons.Program( DataVersionStorage.Update5 )
object GrabIconsU6 extends GrabIcons.Program( DataVersionStorage.Update6 )
object GrabIconsU7 extends GrabIcons.Program( DataVersionStorage.Update7 )
//object GrabIconsU8 extends GrabIcons.Program( DataVersionStorage.Update8 )
