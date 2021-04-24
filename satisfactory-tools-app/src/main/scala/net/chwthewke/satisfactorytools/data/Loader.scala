package net.chwthewke.satisfactorytools
package data

import cats.Monoid
import cats.effect.Blocker
import cats.effect.ContextShift
import cats.effect.IO
import cats.effect.Resource
import cats.effect.Sync
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.validated._
import fs2.Stream
import io.circe.Decoder
import io.circe.fs2.byteArrayParser
import io.circe.fs2.decoder
import java.io.InputStream
import pureconfig.ConfigSource
import pureconfig.module.catseffect.syntax._

import model.Bill
import model.MapOptions
import model.Model
import model.Options
import model.RecipeList
import model.SolverInputs

trait Loader[F[_]] {

  val blocker: Blocker

  implicit val syncInstance: Sync[F]
  implicit val contextShift: ContextShift[F]

  def docsResource: F[InputStream] =
    syncInstance.delay( getClass.getClassLoader.getResourceAsStream( "Docs.json" ) )

  def streamDocsResource: Stream[F, Byte] =
    fs2.io.readInputStream( docsResource, 32768, blocker )

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
    loadResource[GameData]
      .flatMap( data => data.toModel.leftMap( Error( _ ) ).liftTo[F] )

  def loadProductionConfig( src: ConfigSource ): F[ProductionConfig] =
    src.loadF[F, ProductionConfig]( blocker )

  def loadMapOptions( model: Model ): F[MapOptions] =
    Loader.mapConf
      .loadF[F, MapConfig]( blocker )
      .flatMap( MapOptions.init( model, _ ).leftMap( Error( _ ) ).liftTo[F] )

  def loadSolverInputs( model: Model, src: ConfigSource ): F[SolverInputs] =
    for {
      prodConfig <- src.loadF[F, ProductionConfig]( blocker )
      bill       <- Bill.init( model, prodConfig ).leftMap( Error( _ ) ).liftTo[F]
      recipeList <- RecipeList.init( model, prodConfig ).leftMap( Error( _ ) ).liftTo[F]
      mapConfig  <- Loader.mapConf.loadF[F, MapConfig]( blocker )
      mapOptions <- MapOptions.init( model, mapConfig ).leftMap( Error( _ ) ).liftTo[F]
    } yield SolverInputs( bill, recipeList, Options.default, mapOptions )

}

object Loader {

  val mapConf: ConfigSource = ConfigSource.resources( "map.conf" )

  def apply[F[_]]( implicit S: Sync[F], CS: ContextShift[F] ): Resource[F, Loader[F]] = {
    Blocker[F].map(
      blocker0 =>
        new Loader[F] {
          override val blocker: Blocker                       = blocker0
          override implicit val syncInstance: Sync[F]         = S
          override implicit val contextShift: ContextShift[F] = CS
        }
    )
  }

  def io( implicit CS: ContextShift[IO] ): Resource[IO, Loader[IO]] = Loader[IO]

}
