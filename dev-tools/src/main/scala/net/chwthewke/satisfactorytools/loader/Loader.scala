package net.chwthewke.satisfactorytools
package loader

import cats.Monoid
import cats.effect.IO
import cats.effect.Sync
import cats.syntax.all._
import fs2.Stream
import io.circe.Decoder
import io.circe.fs2.byteArrayParser
import io.circe.fs2.decoder
import java.io.InputStream
import pureconfig.ConfigSource
import pureconfig.module.catseffect.syntax._

import data.GameData
import model.Model

class Loader[F[_]]( implicit val syncInstance: Sync[F] ) {

  def docsResource( storage: DataVersionStorage ): F[InputStream] =
    syncInstance.delay( getClass.getClassLoader.getResourceAsStream( s"${storage.docsKey}/Docs.json" ) )

  def streamDocsResource( storage: DataVersionStorage ): Stream[F, Byte] =
    fs2.io.readInputStream( docsResource( storage ), 32768 )

  private def process[A: Decoder: Monoid]( bytes: Stream[F, Byte] ): F[A] =
    bytes
      .through( byteArrayParser[F] )
      .through( decoder[F, A] )
      .compile
      .foldMonoid

  def loadResource[A: Monoid: Decoder]( storage: DataVersionStorage ): F[A] =
    process[A]( streamDocsResource( storage ) )

  def loadGameData( storage: DataVersionStorage ): F[GameData] = loadResource[GameData]( storage )

  def loadModel( storage: DataVersionStorage ): F[Model] =
    (
      loadGameData( storage ),
      loadMapConfig( storage )
    ).tupled
      .flatMap {
        case ( data, map ) => ModelInit( storage.modelVersion, data, map ).leftMap( Error( _ ) ).liftTo[F]
      }

  def loadMapConfig( storage: DataVersionStorage ): F[MapConfig] =
    loadMapConfigs.flatMap(
      _.configs
        .get( storage.modelVersion.version )
        .liftTo[F]( new NoSuchElementException( s"No map config for version ${storage.modelVersion}" ) )
    )

  def loadMapConfigs: F[MapConfigSet] = Loader.mapConf.loadF[F, MapConfigSet]()

}

object Loader {

  val mapConf: ConfigSource = ConfigSource.resources( "map.conf" ).withFallback( ConfigSource.empty )

  def apply[F[_]: Sync]: Loader[F] = new Loader[F]

  val io: Loader[IO] = Loader[IO]

}
