package net.chwthewke.satisfactorytools
package data

import cats.Monoid
import cats.effect.IO
import cats.effect.Sync
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.validated._
import fs2.Stream
import io.circe.Decoder
import java.io.InputStream
import net.chwthewke.vendor.io.circe.fs2.byteArrayParser
import net.chwthewke.vendor.io.circe.fs2.decoder
import pureconfig.ConfigSource
import pureconfig.module.catseffect.syntax._

import model.Bill
import model.Model
import model.Options
import model.RecipeList
import model.SolverInputs

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
      .flatMap { case ( data, map ) => data.toModel( map ).leftMap( Error( _ ) ).liftTo[F] }

  def loadProductionConfig( src: ConfigSource ): F[ProductionConfig] =
    src.loadF[F, ProductionConfig]()

  def loadMapConfig: F[MapConfig] =
    Loader.mapConf.loadF[F, MapConfig]()

  def loadSolverInputs( model: Model, src: ConfigSource ): F[SolverInputs] =
    for {
      prodConfig <- loadProductionConfig( src )
      bill       <- Bill.init( model, prodConfig ).leftMap( Error( _ ) ).liftTo[F]
      recipeList <- RecipeList.init( model, prodConfig ).leftMap( Error( _ ) ).liftTo[F]
    } yield SolverInputs( bill, recipeList, Options.default, model.defaultResourceOptions )

}

object Loader {

  val mapConf: ConfigSource = ConfigSource.resources( "map.conf" )

  def apply[F[_]: Sync]: Loader[F] = new Loader[F]

  val io: Loader[IO] = Loader[IO]

}
