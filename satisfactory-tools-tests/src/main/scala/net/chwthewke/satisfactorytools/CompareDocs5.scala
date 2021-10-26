package net.chwthewke.satisfactorytools

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.apply._
import fr.thomasdufour.autodiff.Pretty

import loader.Loader

object CompareDocs5 extends IOApp {
  import diff._

  val loaderForU4: Loader[IO] = Loader.io
  val loaderForU5: Loader[IO] = new Loader[IO]() { override val docsName: String = "Docs5.json" }

  override def run( args: List[String] ): IO[ExitCode] =
    ( loaderForU4.loadGameData, loaderForU5.loadGameData ).tupled
      .flatTap { case ( u4, u5 ) => IO.println( Pretty.colorized2.show( gameDataDiff.apply( u4, u5 ) ) ) }
      .as( ExitCode.Success )
}
