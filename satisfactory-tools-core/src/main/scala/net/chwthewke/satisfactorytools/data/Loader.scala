package net.chwthewke.satisfactorytools
package data

import cats.Monoid
import cats.effect.Blocker
import cats.effect.ContextShift
import cats.effect.IO
import cats.effect.Sync
import fs2.Stream
import io.circe.Decoder
import io.circe.fs2.byteArrayParser
import io.circe.fs2.decoder
import java.io.InputStream

trait Loader[F[_]] {

  implicit val syncInstance: Sync[F]
  implicit val contextShift: ContextShift[F]

  def docsResource: F[InputStream] =
    syncInstance.delay( getClass.getClassLoader.getResourceAsStream( "Docs.json" ) )

  def streamDocsResource( blocker: Blocker ): Stream[F, Byte] =
    fs2.io.readInputStream( docsResource, 32768, blocker )

  private def process[A: Decoder: Monoid]( bytes: Stream[F, Byte] ): F[A] =
    bytes
      .through( byteArrayParser[F] )
      .through( decoder[F, A] )
      .compile
      .foldMonoid

  def loadResource[A: Monoid: Decoder]( blocker: Blocker ): F[A] =
    process[A]( streamDocsResource( blocker ) )

}

object Loader {

  def apply[F[_]]( implicit S: Sync[F], CS: ContextShift[F] ): Loader[F] =
    new Loader[F] {
      override implicit val syncInstance: Sync[F]         = S
      override implicit val contextShift: ContextShift[F] = CS
    }

  def io( implicit CS: ContextShift[IO] ): Loader[IO] = Loader[IO]

}
