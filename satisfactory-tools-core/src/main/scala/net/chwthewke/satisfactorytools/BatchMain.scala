package net.chwthewke.satisfactorytools

import cats.effect.Blocker
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.instances.string._
import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.show._
import pureconfig.ConfigSource
import pureconfig.module.catseffect.syntax._
//
import load.Loader
import model.ProtoModel
import prod.Bill
import prod.ProductionConfig
import prod.RecipeMatrix

object BatchMain extends IOApp {
  override def run( args: List[String] ): IO[ExitCode] = {
    Blocker[IO]
      .use(
        blocker =>
          (
            Loader.io.loadResource[ProtoModel]( blocker ),
            ConfigSource.default.loadF[IO, ProductionConfig]( blocker )
          ).tupled
      )
      .flatMap {
        case ( proto, config ) =>
          program( proto, config ).as( ExitCode.Success )
      }

  }

  def program( protoModel: ProtoModel, config: ProductionConfig ): IO[Unit] = {

    def result: Either[String, String] =
      for {
        model <- protoModel.toModel
                  .leftMap( errs => show"Errors setting up model:\n  ${errs.map( _.show ).intercalate( "\n  " )}" )
                  .toEither
        bill     <- Bill.init( config, model )
        solution <- RecipeMatrix.init( config, model ).computeFactory( bill )
      } yield solution.show

    IO( println( result.merge ) )
  }

}
