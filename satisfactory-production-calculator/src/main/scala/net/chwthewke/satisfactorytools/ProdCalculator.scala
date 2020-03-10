package net.chwthewke.satisfactorytools

import cats.effect.IO
//
import model.Model
import prod.Calculator
import prod.ProductionConfig

object ProdCalculator extends Program[ProductionConfig] {
  override def runProgram( model: Model, config: ProductionConfig ): IO[Unit] = Calculator[IO]( model, config )
}
