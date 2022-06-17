package net.chwthewke.satisfactorytools

import buildinfo.Satisfactorytools
import cats.effect.ExitCode
import cats.effect.IO
import cats.syntax.either._
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import pureconfig.module.catseffect.syntax._

import loader.Loader
import model.ExtractorType
import model.Options
import prod.Calculator
import prod.ConstraintSolver
import prod.SolverInputs

object ProdCalculator
    extends CommandIOApp(
      "prod-calculator",
      "Production Calculator (LP edition)",
      version = Satisfactorytools.shortVersion
    ) {

  override def main: Opts[IO[ExitCode]] =
    Program.configOpt.map(
      cfg =>
        for {
          model      <- Loader.io.loadModel( DataVersionStorage.Update5 )
          prodConfig <- cfg.loadF[IO, ProductionConfig]()
          bill       <- prodConfig.mkBill( model ).leftMap( Error( _ ) ).liftTo[IO]
          recipeList <- prodConfig.mkRecipeList( model ).leftMap( Error( _ ) ).liftTo[IO]
          _ <- IO.println(
                Calculator(
                  model,
                  SolverInputs( bill, recipeList, myOptions, model.defaultResourceOptions ),
                  ConstraintSolver
                )
              )
        } yield ExitCode.Success
    )

  private val myOptions = Options(
    Options.Belt.BeltMk5,
    Options.Pipe.PipeMk2,
    Options.Miner.MinerMk3,
    Options.ClockSpeed.ClockSpeed100,
    ExtractorType.values.toSet,
    Set.empty
  )
}
