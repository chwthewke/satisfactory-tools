package net.chwthewke.satisfactorytools.load

import cats.effect.Blocker
import cats.effect.ContextShift
import cats.effect.IO
import cats.effect.Sync
import fs2.Stream
import fs2.io.file
import io.circe.Json
import io.circe.fs2.byteArrayParser
import io.circe.fs2.decoder
import java.nio.file.Path
import java.nio.file.Paths
import net.chwthewke.satisfactorytools.model.Model

trait Loader[F[_]] {

  val blocker: Blocker
  implicit val syncInstance: Sync[F]
  implicit val contextShift: ContextShift[F]

  val defaultPath: Path = Paths.get( "C:\\Users\\chwth\\Desktop\\Docs.json" )

  def streamDocsJson( path: Path = defaultPath ): Stream[F, Json] =
    file
      .readAll( path, blocker, 32768 )
      .through( byteArrayParser[F] )

  def load( path: Path = defaultPath ): F[Model] =
    streamDocsJson( path )
      .through( decoder[F, Model] )
      .compile
      .foldMonoid

}

object Loader {

  def apply[F[_]]( blocker0: Blocker )( implicit S: Sync[F], CS: ContextShift[F] ): Loader[F] =
    new Loader[F] {
      override val blocker: Blocker                       = blocker0
      override implicit val syncInstance: Sync[F]         = S
      override implicit val contextShift: ContextShift[F] = CS
    }

  def io( blocker: Blocker )( implicit CS: ContextShift[IO] ): Loader[IO] = Loader[IO]( blocker )

}
