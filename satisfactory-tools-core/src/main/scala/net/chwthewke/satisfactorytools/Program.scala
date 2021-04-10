package net.chwthewke.satisfactorytools

import cats.effect.Blocker
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.either._
import pureconfig.ConfigReader
import pureconfig.ConfigSource
import pureconfig.module.catseffect.syntax._
import scala.reflect.ClassTag

import data.GameData
import data.Loader
import data.MapConfig
import model.Model

abstract class Program[Cfg: ConfigReader: ClassTag] extends IOApp {
  def runProgram( model: Model, config: Cfg ): IO[Unit]

  def loadConfig[C: ConfigReader: ClassTag]( blocker: Blocker, source: ConfigSource ): IO[C] =
    source.loadF[IO, C]( blocker )

  def loadModel( blocker: Blocker, map: MapConfig ): IO[Model] =
    Loader.io
      .loadResource[GameData]( blocker )
      .flatMap(
        proto => proto.toModel( map ).toEither.leftMap( Error( _ ) ).liftTo[IO]
      )

  def loadAll: IO[( Model, Cfg )] =
    Blocker[IO].use(
      blocker =>
        for {
          config <- loadConfig[Cfg]( blocker, ConfigSource.default )
          map    <- loadConfig[MapConfig]( blocker, ConfigSource.resources( "map.conf" ) )
          data   <- loadModel( blocker, map )
        } yield ( data, config )
    )

  override final def run( args: List[String] ): IO[ExitCode] =
    for {
      ( model, config ) <- loadAll
      _                 <- runProgram( model, config )
    } yield ExitCode.Success

}
