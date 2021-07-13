package net.chwthewke.satisfactorytools
package web

import cats.Defer
import cats.Monad
import cats.effect.Async
import cats.effect.ExitCode
import cats.effect.Ref
import cats.effect.Resource
import cats.effect.Sync
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.semigroupk._
import fs2.concurrent.SignallingRef
import org.http4s.HttpApp
import org.http4s.HttpRoutes
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.dsl.Http4sDsl
import org.http4s.server.middleware.AutoSlash
import org.http4s.syntax.kleisli._
import scala.concurrent.ExecutionContext

import loader.Loader
import model.Bill
import model.Model
import model.Options
import model.RecipeList
import prod.SolverInputs
import web.session.SessionMiddleware

object WebApp {

  def httpApp[F[_]: Async]( shutdownCommand: Ref[F, Boolean] ): Resource[F, HttpApp[F]] =
    (
      Resource.eval( loadAll[F] ),
      Resource.eval( SessionMiddleware.init[F] )
    ).mapN {
      case ( ( model, inputs ), sessions ) =>
        AutoSlash
          .httpRoutes( sessions( Pages( sessions.sessionKey, model, inputs ).routes ) <+> shutdown( shutdownCommand ) )
          .orNotFound
    }

  private def shutdown[F[_]: Defer: Monad]( shutdownCommand: Ref[F, Boolean] ): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl._

    HttpRoutes.of[F] {
      case _ -> Root / "shutdown" => shutdownCommand.set( true ) *> Ok()
    }
  }

  def server[F[_]: Async]: F[ExitCode] = {
    Resource.eval( SignallingRef[F, Boolean]( false ) ).mproduct( httpApp[F] ).use {
      case ( signal, app ) =>
        Ref
          .of[F, ExitCode]( ExitCode.Success )
          .flatMap(
            exitRef =>
              BlazeServerBuilder[F]( ExecutionContext.global )
                .withHttpApp( app )
                .bindHttp( port = 7284 )
                .withWebSockets( false )
                .withLengthLimits( 65536, 65536 )
                .serveWhile( signal, exitRef )
                .compile
                .lastOrError
          )
    }
  }

  def loadAll[F[_]: Sync]: F[( Model, SolverInputs )] =
    Loader[F].loadModel
      .fproduct(
        model =>
          SolverInputs(
            Bill.empty,
            RecipeList( model.manufacturingRecipes ),
            Options.default,
            model.defaultResourceOptions
          )
      )

}
