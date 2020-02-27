package net.chwthewke.satisfactorytools

import cats.effect.Blocker
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.instances.string._
import cats.instances.vector._
import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.show._
import java.nio.file.Files
import java.nio.file.Paths
import pureconfig.ConfigSource
import pureconfig.module.catseffect.syntax._
//
import load.Loader
import model.ProtoModel
import prod.ProductionConfig
import prod.RecipeMatrix

object DumpMatrixCsv extends IOApp {
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
        case ( proto, config ) => program( proto, config )
      }
  }

  def program( proto: ProtoModel, config: ProductionConfig ): IO[ExitCode] = {
    proto.toModel
      .leftMap( errs => show"Errors setting up model:\n  ${errs.map( _.show ).intercalate( "\n  " )}" )
      .toEither
      .map { model =>
        val matrix = RecipeMatrix.init( config, model )

        (
          ("" +: matrix.columnLabels.map( _.displayName )).intercalate( "," ) +:
            matrix.rowLabels.zipWithIndex.map {
              case ( it, ix ) =>
                (
                  it.displayName +: (0 until matrix.matrix.cols)
                    .map( matrix.matrix( ix, _ ) )
                    .map( v => f"$v%4.3f" )
                    .toVector
                ).intercalate( "," )
            }
        ).intercalate( "\n" )
      }
      .fold(
        err => IO( println( err ) ).as( ExitCode.Error ),
        ok => IO( Files.writeString( Paths.get( "recipes.csv" ), ok ) ).as( ExitCode.Success )
      )

  }

}
