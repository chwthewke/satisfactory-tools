package net.chwthewke.satisfactorytools

import buildinfo.Satisfactorytools
import cats.effect.Blocker
import cats.effect.ExitCode
import cats.effect.IO
import cats.syntax.either._
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import pureconfig.ConfigReader
import pureconfig.ConfigSource
import pureconfig.module.catseffect.syntax._
import scala.reflect.ClassTag

import data.GameData
import data.Loader
import data.MapConfig
import model.Model

abstract class Program[Cfg: ConfigReader: ClassTag]( name: String, header: String )
    extends CommandIOApp( name, header, version = Satisfactorytools.shortVersion ) {
  def runProgram( model: Model, config: Cfg ): IO[Unit]

  def loadModel( blocker: Blocker, map: MapConfig ): IO[Model] =
    Loader.io
      .loadResource[GameData]( blocker )
      .flatMap(
        proto => proto.toModel( map ).toEither.leftMap( Error( _ ) ).liftTo[IO]
      )

  def loadAll( configSrc: ConfigSource ): IO[( Model, Cfg )] =
    Blocker[IO].use(
      blocker =>
        for {
          config <- configSrc.loadF[IO, Cfg]( blocker )
          map    <- ConfigSource.resources( "map.conf" ).loadF[IO, MapConfig]( blocker )
          data   <- loadModel( blocker, map )
        } yield ( data, config )
    )

  override def main: Opts[IO[ExitCode]] =
    Program.configOpt.map(
      configSource =>
        for {
          ( model, config ) <- loadAll( configSource )
          _                 <- runProgram( model, config )
        } yield ExitCode.Success
    )

}

object Program {
  def configOpt: Opts[ConfigSource] =
    Opts
      .argument[String]( "CONFIG" )
      .orNone
      .map(
        _.fold( ConfigSource.default )(
          name =>
            ConfigSource
              .resources( s"$name.conf" )
              .withFallback( ConfigSource.defaultReference )
        )
      )
}
