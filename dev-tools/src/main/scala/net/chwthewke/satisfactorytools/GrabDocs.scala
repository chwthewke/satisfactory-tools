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
import fs2.Stream
import fs2.io.file.Path
import fs2.io.file.Files
import fs2.text
import fs2.text.utf8
import io.circe.parser
import java.nio.charset.StandardCharsets

class GrabDocs[F[_]: Sync]( implicit F: Files[F] ) {
  import GrabDocs._

  private val console: Console[F] = Console.make[F]

  def run( storage: DataVersionStorage ): F[Unit] =
    storage.gameSource.fold( console.println( show"No Docs.json source for ${storage.entryName}" ) ) { source =>
      val docsPath: Path = source.path
        .resolve( "CommunityResources" )
        .resolve( "Docs" )
        .resolve( "Docs.json" )

      val destDir: Path  = destBase.resolve( storage.docsPath )
      val destPath: Path = destDir.resolve( "Docs.json" )

      for {
        contents <- F.readAll( docsPath )
                     .through( text.decodeWithCharset[F]( StandardCharsets.UTF_16 ) )
                     .compile
                     .foldMonoid
        json <- parser.parse( contents ).liftTo[F]
        _    <- console.println( show"Loaded ${docsPath.toString} and parsed as JSON." )
        _    <- F.createDirectories( destDir )
        _ <- Stream
              .emit[F, String]( json.spaces2SortKeys )
              .through( utf8.encode[F] )
              .through( F.writeAll( destPath ) )
              .compile
              .drain
        _ <- console.println( show"Prettified and wrote ${destPath.toString} as UTF-8." )
      } yield ()
    }

}

object GrabDocs {
  val destBase: Path = Path( "dev-tools" ) / "src" / "main" / "resources"

  abstract class Program( storage: DataVersionStorage ) extends IOApp {
    override def run( args: List[String] ): IO[ExitCode] =
      new GrabDocs[IO].run( storage ).as( ExitCode.Success )
  }
}

object GrabDocsU4 extends GrabDocs.Program( DataVersionStorage.Update4 )
object GrabDocsU5 extends GrabDocs.Program( DataVersionStorage.Update5 )
object GrabDocsU6 extends GrabDocs.Program( DataVersionStorage.Update6 )
