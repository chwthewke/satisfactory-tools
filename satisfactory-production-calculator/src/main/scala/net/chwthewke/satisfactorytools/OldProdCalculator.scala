package net.chwthewke.satisfactorytools

import cats.effect.IO

import data.ProductionConfig
import model.Model
import model.Options
import prod.Calculator
import prod.RecipeMatrix

object OldProdCalculator extends Program[ProductionConfig] {
  override def runProgram( model: Model, config: ProductionConfig ): IO[Unit] =
    IO.delay( println( Calculator[IO]( model, config, Options.default, RecipeMatrix ) ) )
}
