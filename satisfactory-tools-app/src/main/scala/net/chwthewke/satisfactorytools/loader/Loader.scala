package net.chwthewke.satisfactorytools
package loader

import cats.Monoid
import cats.effect.IO
import cats.effect.Sync
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.validated._
import fs2.Stream
import io.circe.Decoder
import java.io.InputStream
import net.chwthewke.vendor.io.circe.fs2.byteArrayParser
import net.chwthewke.vendor.io.circe.fs2.decoder
import pureconfig.ConfigSource
import pureconfig.module.catseffect.syntax._

import data.GameData
import model.MapConfig
import model.Model

class Loader[F[_]]( implicit val syncInstance: Sync[F] ) {

  def docsResource: F[InputStream] =
    syncInstance.delay( getClass.getClassLoader.getResourceAsStream( "Docs.json" ) )

  def streamDocsResource: Stream[F, Byte] =
    fs2.io.readInputStream( docsResource, 32768 )

  private def process[A: Decoder: Monoid]( bytes: Stream[F, Byte] ): F[A] =
    bytes
      .through( byteArrayParser[F] )
      .through( decoder[F, A] )
      .compile
      .foldMonoid

  def loadResource[A: Monoid: Decoder]: F[A] =
    process[A]( streamDocsResource )

  def loadGameData: F[GameData] = loadResource[GameData]

  def loadModel: F[Model] =
    ( loadResource[GameData], loadMapConfig ).tupled
      .flatMap { case ( data, map ) => Model.init( data, map ).leftMap( Error( _ ) ).liftTo[F] }

  def loadMapConfig: F[MapConfig] =
    Loader.mapConf.loadF[F, MapConfig]()

}

object Loader {

  val mapConf: ConfigSource = ConfigSource.resources( "map.conf" ).withFallback( ConfigSource.empty )

  def apply[F[_]: Sync]: Loader[F] = new Loader[F]

  val io: Loader[IO] = Loader[IO]

}
