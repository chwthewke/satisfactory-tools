package net.chwthewke.satisfactorytools
package prod

import cats.effect.kernel.Sync
import cats.syntax.either._
import cats.syntax.flatMap._
import scala.util.control.NoStackTrace

import model.Bill

trait Solver[F[_]] {
  def solve( bill: Bill, recipes: RecipeSelection ): F[Solution]
}

object Solver {
  def blocking[F[_]: Sync]( solver: Solver[Either[String, *]] ): Solver[F] =
    new Blocking[F]( solver )

  private class Blocking[F[_]]( val delegate: Solver[Either[String, *]] )( implicit F: Sync[F] ) extends Solver[F] {
    def solve( bill: Bill, recipes: RecipeSelection ): F[Solution] =
      F.interruptible( delegate.solve( bill, recipes ) )
        .flatMap( _.leftMap( Error ).liftTo[F] )
  }

  case class Error( message: String ) extends RuntimeException( message ) with NoStackTrace
}
