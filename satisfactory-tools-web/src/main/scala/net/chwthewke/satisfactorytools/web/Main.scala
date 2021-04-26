package net.chwthewke.satisfactorytools
package web

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp

object Main extends IOApp {
  override def run( args: List[String] ): IO[ExitCode] = WebApp.server[IO]
}
