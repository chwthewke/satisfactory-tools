package net.chwthewke.satisfactorytools

import cats.effect.IO

import data.ProductionConfig
import model.Model
import model.Options
import prod.Calculator
import prod.ConstraintSolver

object ProdCalculator extends Program[ProductionConfig]( "prod-calculator", "Production Calculator (LP edition)" ) {
  override def runProgram( model: Model, config: ProductionConfig ): IO[Unit] =
    IO.delay( println( Calculator[IO]( model, config, myOptions, ConstraintSolver ) ) )

  private val myOptions = Options(
    Options.Belt.BeltMk5,
    Options.Pipe.PipeMk2,
    Options.Miner.MinerMk3,
    Options.ClockSpeed.ClockSpeed100,
    Options.Extractors.values.toSet,
    Set.empty
  )
}
