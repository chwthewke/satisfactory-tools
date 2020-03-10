package net.chwthewke.satisfactorytools

import cats.effect.Blocker
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.apply._
import cats.syntax.either._
import pureconfig.ConfigReader
import pureconfig.ConfigSource
import pureconfig.module.catseffect.syntax._
import scala.reflect.ClassTag
//
import net.chwthewke.satisfactorytools.load.Loader
import net.chwthewke.satisfactorytools.model.Model
import net.chwthewke.satisfactorytools.model.ProtoModel

abstract class Program[Cfg: ConfigReader: ClassTag] extends IOApp {
  def runProgram( model: Model, config: Cfg ): IO[Unit]

  def loadConfig( blocker: Blocker ): IO[Cfg] =
    ConfigSource.default.loadF[IO, Cfg]( blocker )

  def loadModel( blocker: Blocker ): IO[Model] =
    Loader.io
      .loadResource[ProtoModel]( blocker )
      .flatMap(
        proto => proto.toModel.toEither.leftMap( Error( _ ) ).liftTo[IO]
      )

  override final def run( args: List[String] ): IO[ExitCode] =
    for {
      ( model, config ) <- Blocker[IO].use( blocker => ( loadModel( blocker ), loadConfig( blocker ) ).tupled )
      _                 <- runProgram( model, config )
    } yield ExitCode.Success

}
