package net.chwthewke.satisfactorytools
package prod

import cats.effect.Sync
import cats.syntax.show._
import model.Model

object Calculator {
  def apply[F[_]]( model: Model, config: ProductionConfig )( implicit F: Sync[F] ): F[Unit] = {
    def result: Either[String, String] =
      for {
        bill     <- Bill.init( config, model )
        solution <- RecipeMatrix.init( config, model ).computeFactory( bill )
      } yield solution.show

    F.delay( println( result.merge ) )
  }
}
