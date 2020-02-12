package net.chwthewke.satisfactorytools

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.functor._

object Main extends IOApp {

  override def run( args: List[String] ): IO[ExitCode] =
    IO( println( s"${buildinfo.Satisfactorytools.name} ${buildinfo.Satisfactorytools.version}" ) )
      .as( ExitCode.Success )

}
