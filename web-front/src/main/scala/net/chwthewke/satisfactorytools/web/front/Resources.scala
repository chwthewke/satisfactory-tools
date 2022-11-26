package net.chwthewke.satisfactorytools
package web.front

import cats.MonadThrow
import cats.effect.Async
import cats.effect.MonadCancelThrow
import cats.syntax.either._
import cats.syntax.functor._
import org.http4s.client.Client
import org.http4s.client.Middleware
import org.http4s.dom.FetchClientBuilder
import org.scalajs.dom.RequestMode

object Resources {

  private def baseUri[F[_]: MonadCancelThrow]( config: Config ): Middleware[F] =
    (client: Client[F]) => Client[F]( req => client.run( req.withUri( config.baseUri.resolve( req.uri ) ) ) )

  def httpClientIn[F[_]: Async, G[_]: MonadThrow]: G[Client[F]] =
    Config.load
      .liftTo[G]
      .map( baseUri[F]( _ ).apply( FetchClientBuilder[F].withMode( RequestMode.`same-origin` ).create ) )

}
