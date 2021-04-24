package net.chwthewke.satisfactorytools

import buildinfo.Satisfactorytools
import cats.effect.ExitCode
import cats.effect.IO
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp

import data.Loader
import model.Options
import prod.Calculator
import prod.ConstraintSolver

object ProdCalculator
    extends CommandIOApp(
      "prod-calculator",
      "Production Calculator (LP edition)",
      version = Satisfactorytools.shortVersion
    ) {

  override def main: Opts[IO[ExitCode]] =
    Program.configOpt.map(
      cfg =>
        Loader.io.use(
          loader =>
            for {
              model  <- loader.loadModel
              inputs <- loader.loadSolverInputs( model, cfg )
              _      <- IO.delay( println( Calculator[IO]( model, inputs.copy( options = myOptions ), ConstraintSolver ) ) )
            } yield ExitCode.Success
        )
    )

  private val myOptions = Options(
    Options.Belt.BeltMk5,
    Options.Pipe.PipeMk2,
    Options.Miner.MinerMk3,
    Options.ClockSpeed.ClockSpeed100,
    Options.Extractors.values.toSet,
    Set.empty
  )
}
