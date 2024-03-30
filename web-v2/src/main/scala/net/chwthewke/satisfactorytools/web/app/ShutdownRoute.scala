package net.chwthewke.satisfactorytools
package web.app

import cats.effect.Async
import cats.effect.Ref
import cats.effect.syntax.spawn._
import cats.effect.syntax.temporal._
import cats.syntax.apply._
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import scala.concurrent.duration._
import shapeless.tag
import shapeless.tag.@@

import web.view.ShutdownView

class ShutdownRoute[F[_]: Async] extends Http4sDsl[F] {

  def route( shutdownCommand: Ref[F, Boolean] ): HttpRoutes[F] = {
    HttpRoutes.of[F] {
      case _ -> Root / "shutdown" =>
        Ok( ShutdownView() ) <*
          shutdownCommand.set( true ).delayBy( 1.second ).start
    }
  }
}

object ShutdownRoute {
  trait Tag
  def apply[F[_]: Async]( shutdownCommand: Ref[F, Boolean] ): HttpRoutes[F] @@ ShutdownRoute.Tag =
    tag[Tag]( new ShutdownRoute[F].route( shutdownCommand ) )
}
