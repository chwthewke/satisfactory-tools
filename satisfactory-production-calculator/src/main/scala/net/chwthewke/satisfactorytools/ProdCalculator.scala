package net.chwthewke.satisfactorytools

import cats.effect.IO

import data.ProductionConfig
import model.Model
import model.Options
import prod.Calculator
import prod.RecipeMatrix

object ProdCalculator extends Program[ProductionConfig] {
  override def runProgram( model: Model, config: ProductionConfig ): IO[Unit] =
    Calculator[IO]( model, config, Options.default, RecipeMatrix )
}
