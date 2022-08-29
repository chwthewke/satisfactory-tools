package net.chwthewke.satisfactorytools
package web
package server

import cats.effect.Async
import cats.effect.ExitCode
import cats.effect.Resource
import cats.syntax.flatMap._
import pureconfig.ConfigSource
import pureconfig.module.catseffect.syntax._

object Application {
  def runServer[F[_]: Async]( jsFiles: Vector[String] ): F[ExitCode] =
    Resource
      .eval( ConfigSource.default.loadF[F, Config]() )
      .mproduct( config => Service[F]( jsFiles, config.db ) )
      .use {
        case ( config, service ) =>
          Server.run[F]( service.route.orNotFound, config.port )
      }
}
