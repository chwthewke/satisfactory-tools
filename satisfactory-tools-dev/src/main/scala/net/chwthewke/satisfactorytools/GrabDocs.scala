package net.chwthewke.satisfactorytools

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.Sync
import cats.effect.std.Console
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.show._
import io.circe.parser
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.StandardOpenOption._

class GrabDocs[F[_]]( implicit F: Sync[F] ) {
  import GrabDocs._

  private val console: Console[F] = Console.make[F]

  def run( storage: DataVersionStorage ): F[Unit] =
    storage.gameSource.fold( console.println( show"No Docs.json source for ${storage.entryName}" ) ) { source =>
      val docsPath: Path = source
        .resolve( "CommunityResources" )
        .resolve( "Docs" )
        .resolve( "Docs.json" )

      val destDir: Path  = destBase.resolve( storage.docsPath )
      val destPath: Path = destDir.resolve( "Docs.json" )

      for {
        contents <- F.delay( Files.readString( docsPath, StandardCharsets.UTF_16 ) )
        json     <- parser.parse( contents ).liftTo[F]
        _        <- console.println( show"Loaded ${docsPath.toString} and parsed as JSON." )
        _        <- F.delay( Files.createDirectories( destDir ) )
        _ <- F.delay(
              Files.writeString( destPath, json.spaces2SortKeys, StandardCharsets.UTF_8, CREATE, TRUNCATE_EXISTING )
            )
        _ <- console.println( show"Prettified and wrote ${destPath.toString} as UTF-8." )
      } yield ()
    }

}

object GrabDocs {
  val destBase: Path = Paths.get( "satisfactory-tools-dev", "src", "main", "resources" )

  abstract class Program( storage: DataVersionStorage ) extends IOApp {
    override def run( args: List[String] ): IO[ExitCode] =
      new GrabDocs[IO].run( storage ).as( ExitCode.Success )
  }
}

object GrabDocsU4 extends GrabDocs.Program( DataVersionStorage.Update4 )
object GrabDocsU5 extends GrabDocs.Program( DataVersionStorage.Update5 )
object GrabDocsU6 extends GrabDocs.Program( DataVersionStorage.Update6 )
