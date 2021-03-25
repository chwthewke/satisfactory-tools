package net.chwthewke.satisfactorytools
package prod

import cats.effect.Sync
import cats.syntax.show._
import model.Model

object Calculator {
  def apply[F[_]]( model: Model, config: ProductionConfig )( implicit F: Sync[F] ): F[Unit] =
    F.delay( println( computeFactory( model, config ).map( _.show ).merge ) )

  def computeFactory( model: Model, config: ProductionConfig ): Either[String, Factory] =
    for {
      bill     <- Bill.init( config, model )
      solution <- RecipeMatrix.init( config, model ).computeFactory( bill )
    } yield solution

}
