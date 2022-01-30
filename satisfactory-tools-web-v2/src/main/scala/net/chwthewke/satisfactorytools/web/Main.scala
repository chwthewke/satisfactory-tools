package net.chwthewke.satisfactorytools
package web

import cats.effect.Async
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.Ref
import cats.effect.Resource
import cats.syntax.apply._
import cats.syntax.semigroupk._
import doobie.Transactor
import fs2.concurrent.Signal
import fs2.concurrent.SignallingRef
import org.http4s.HttpApp
import org.http4s.HttpRoutes
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.dsl.Http4sDsl
import org.http4s.server.middleware.AutoSlash
import pureconfig.ConfigSource
import pureconfig.module.catseffect.syntax._

import persistence.Library
import persistence.PersistentLoader
import persistence.Plans
import persistence.Sessions
import web.app.Application

class Main[F[_]: Async] {

  private def shutdown( shutdownCommand: Ref[F, Boolean] ): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl._

    HttpRoutes.of[F] {
      case _ -> Root / "shutdown" => shutdownCommand.set( true ) *> Ok()
    }
  }

  private val mkShutdownSignal: Resource[F, SignallingRef[F, Boolean]] =
    Resource.eval( SignallingRef[F, Boolean]( false ) )

  private val mkTransactor: Resource[F, Transactor[F]] =
    Resource
      .eval( ConfigSource.default.loadF[F, Config]() )
      .flatMap( cfg => persistence.Resources.managedTransactor[F]( cfg.db ) )

  val httpApp: Resource[F, ( Signal[F, Boolean], HttpApp[F] )] =
    for {
      shutdownSignal <- mkShutdownSignal
      xa             <- mkTransactor
      model          <- Resource.eval( PersistentLoader.Doobie.mapK( xa.trans ).loadModel )
    } yield {
      val app = Application[F](
        model,
        Sessions.mapK( xa.trans ),
        Library.mapK( xa.trans ),
        Plans.mapK( xa.trans )
      )

      (
        shutdownSignal,
        AutoSlash.httpRoutes( app.routes <+> shutdown( shutdownSignal ) ).orNotFound
      )

    }

}

object Main extends IOApp {
  override def run( args: List[String] ): IO[ExitCode] =
    new Main[IO].httpApp.use {
      case ( signal, app ) =>
        Ref[IO]
          .of( ExitCode.Success )
          .flatMap(
            exitRef =>
              BlazeServerBuilder[IO]
                .withHttpApp( app )
                .bindHttp( port = 7282 )
                .serveWhile( signal, exitRef )
                .compile
                .lastOrError
          )
    }
}
