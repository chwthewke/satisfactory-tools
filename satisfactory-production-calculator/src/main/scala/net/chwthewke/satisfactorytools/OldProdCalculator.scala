package net.chwthewke.satisfactorytools

import buildinfo.Satisfactorytools
import cats.effect.ExitCode
import cats.effect.IO
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp

import data.Loader
import prod.Calculator
import prod.RecipeMatrix

object OldProdCalculator
    extends CommandIOApp(
      "prod-calculator-old",
      "Production Calculator (Matrix edition)",
      version = Satisfactorytools.shortVersion
    ) {

  override def main: Opts[IO[ExitCode]] =
    Program.configOpt.map(
      cfg =>
        for {
          model  <- Loader.io.loadModel
          inputs <- Loader.io.loadSolverInputs( model, cfg )
          _      <- IO.println( Calculator[IO]( model, inputs, RecipeMatrix ) )
        } yield ExitCode.Success
    )

}
