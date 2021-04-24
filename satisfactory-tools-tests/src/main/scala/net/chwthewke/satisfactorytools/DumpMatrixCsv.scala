package net.chwthewke.satisfactorytools

import buildinfo.Satisfactorytools
import cats.effect.ExitCode
import cats.effect.IO
import cats.syntax.foldable._
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp

import data.Loader
import data.ProductionConfig
import model.Model

object DumpMatrixCsv
    extends CommandIOApp(
      "dump-matrix-csv",
      "Dump recipe matrix as csv",
      version = Satisfactorytools.shortVersion
    ) {

  override def main: Opts[IO[ExitCode]] =
    Program.configOpt.map(
      cfg =>
        Loader.io.use(
          loader =>
            for {
              model      <- loader.loadModel
              prodConfig <- loader.loadProductionConfig( cfg )
              _          <- runProgram( model, prodConfig )
            } yield ExitCode.Success
        )
    )

  def runProgram( model: Model, config: ProductionConfig ): IO[Unit] = {

    val matrix = MkRecipeMatrix( config, model )

    val output = (
      ("" +: matrix.columnLabels.map( _.displayName )).intercalate( "," ) +:
        matrix.rowLabels.zipWithIndex.map {
          case ( it, ix ) =>
            (
              it.displayName +: (0L until matrix.matrix.countColumns)
                .map( matrix.matrix.get( ix.toLong, _ ) )
                .map( v => f"$v%4.3f" )
                .toVector
            ).intercalate( "," )
        }
    ).intercalate( "\n" )

    IO.delay( println( output ) )
  }

}
