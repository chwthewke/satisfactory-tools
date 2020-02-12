package net.chwthewke.satisfactorytools

import cats.effect.Blocker
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.Sync
//import cats.instances.vector._
//import cats.syntax.applicative._
//import cats.syntax.apply._
//import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.show._
//import fs2.Stream
//import io.circe.Decoder
import io.circe.fs2.decoder

object Explore extends IOApp {
  override def run( args: List[String] ): IO[ExitCode] =
    Blocker[IO].use(
      blocker =>
        Loader
          .io( blocker )
          .streamDocsJson()
          .through( decoder( Sync[IO], DocsJsonDecoders.decodeRecipeData ) )
          .foldMonoid
          .compile
          .lastOrError
          .flatMap( rd => IO( println( rd.show ) ) )
          .as( ExitCode.Success )
    )

}
