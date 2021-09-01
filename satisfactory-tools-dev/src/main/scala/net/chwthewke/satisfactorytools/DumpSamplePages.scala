package net.chwthewke.satisfactorytools

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.kernel.Async
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import fs2.Stream
import fs2.io.file.Files
import fs2.io.file.Path
import fs2.text.utf8
import java.time.Instant
import scalatags.Text.Tag

import protocol.PlanHeader
import protocol.PlanId
import protocol.PlanName
import protocol.SolutionHeader
import protocol.SolutionId
import protocol.UserId
import web.view.LibraryView

object DumpSamplePages extends IOApp {

  private final class Program[F[_]: Async] {

    def writePage( page: Tag, path: Path ): F[Unit] = {

      Stream
        .emit( page.render )
        .covary[F]
        .through( utf8.encode )
        .through( Files[F].writeAll( path ) )
        .compile
        .drain
    }

    private val solutionHeader: SolutionHeader[SolutionId] =
      SolutionHeader.Computed( SolutionId( 1 ), 4, 4 )

    private val planHeader: PlanHeader =
      PlanHeader(
        PlanId( 1 ),
        UserId( 1 ),
        Some( PlanName( "test" ) ),
        Some( PlanId( 0 ) ),
        Instant.parse( "2020-01-01T00:00:00Z" ),
        solutionHeader
      )

    val pages: Vector[( String, F[Tag] )] =
      Vector(
        "library" -> LibraryView.viewAllPlans( Vector( planHeader, planHeader ) ).pure[F],
        "delete"  -> LibraryView.deleteConfirm( planHeader ).pure[F]
      )

    val run: F[Unit] =
      pages.traverse_ { case ( name, tag ) => tag.flatMap( writePage( _, Path.apply( s"$name.html" ) ) ) }

  }

  override def run( args: List[String] ): IO[ExitCode] =
    new Program[IO].run.as( ExitCode.Success )
}
