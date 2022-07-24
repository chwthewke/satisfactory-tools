package net.chwthewke.satisfactorytools
package web
package server

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp

object Main extends IOApp {
  override def run( args: List[String] ): IO[ExitCode] =
    Server.run[IO]( WebServerBuildInfo.jsResources.toVector, 7285 )
}
