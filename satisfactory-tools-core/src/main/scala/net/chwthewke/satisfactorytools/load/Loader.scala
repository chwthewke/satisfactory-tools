package net.chwthewke.satisfactorytools
package load

import cats.Monoid
import cats.effect.Blocker
import cats.effect.ContextShift
import cats.effect.IO
import cats.effect.Sync
import fs2.Stream
import fs2.io.file
import io.circe.Decoder
import io.circe.fs2.byteArrayParser
import io.circe.fs2.decoder
import java.io.InputStream
import java.nio.file.Path
import java.nio.file.Paths

trait Loader[F[_]] {

  implicit val syncInstance: Sync[F]
  implicit val contextShift: ContextShift[F]

  val defaultPath: Path = Paths.get( "C:\\Users\\chwth\\Desktop\\Docs.json" )

  def docsResource: F[InputStream] =
    syncInstance.delay( getClass.getClassLoader.getResourceAsStream( "Docs.json" ) )

  def streamDocsResource( blocker: Blocker ): Stream[F, Byte] =
    fs2.io.readInputStream( docsResource, 32768, blocker )

  def streamDocsFile( blocker: Blocker, path: Path = defaultPath ): Stream[F, Byte] =
    file
      .readAll( path, blocker, 32768 )

  private def process[A: Decoder: Monoid]( bytes: Stream[F, Byte] ): F[A] =
    bytes
      .through( byteArrayParser[F] )
      .through( decoder[F, A] )
      .compile
      .foldMonoid

  def loadResource[A: Monoid: Decoder]( blocker: Blocker ): F[A] =
    process[A]( streamDocsResource( blocker ) )

  def loadFile[A: Monoid: Decoder]( blocker: Blocker, path: Path = defaultPath ): F[A] =
    process[A]( streamDocsFile( blocker, path ) )

}

object Loader {

  def apply[F[_]]( implicit S: Sync[F], CS: ContextShift[F] ): Loader[F] =
    new Loader[F] {
      override implicit val syncInstance: Sync[F]         = S
      override implicit val contextShift: ContextShift[F] = CS
    }

  def io( implicit CS: ContextShift[IO] ): Loader[IO] = Loader[IO]

}
