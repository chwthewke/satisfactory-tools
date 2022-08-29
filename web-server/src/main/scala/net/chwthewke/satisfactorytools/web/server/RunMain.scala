package net.chwthewke.satisfactorytools
package web
package server

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp

abstract class RunMain( jsFiles: Vector[String] ) extends IOApp {
  override def run( args: List[String] ): IO[ExitCode] =
    Application.runServer[IO]( jsFiles )
}
