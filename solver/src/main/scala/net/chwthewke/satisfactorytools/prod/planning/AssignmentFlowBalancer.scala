package net.chwthewke.satisfactorytools
package prod
package planning

import cats.Monad
import cats.data.NonEmptyVector
import cats.effect.kernel.Sync
import cats.syntax.align._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.functorFilter._
import cats.syntax.nested._
import scala.collection.immutable.SortedMap

object AssignmentFlowBalancer {

  def apply[F[_]: Sync]: AssignmentFlowBalancer[F] =
    new AssignmentFlowBalancer( new AssignmentSolver.Combined[F] )

  def normalizedNoPrefs( ins: Vector[Double], outs: Vector[Double] ): Vector[SortedMap[Int, Double]] =
    ins.map( in =>
      outs.zipWithIndex
        .mapFilter {
          case ( out, j ) =>
            val a: Double = in * out
            Option.when( a.abs > 0 )( ( j, a ) )
        }
        .to( SortedMap )
    )

}

class AssignmentFlowBalancer[F[_]: Monad]( val solver: AssignmentSolver[F] ) extends FlowBalancer[F] {
  import FlowBalancer.Tolerance

  def balance(
      ins: Vector[Double],
      outs: Vector[Double],
      prefs: Vector[NonEmptyVector[( Int, Int )]]
  ): F[Vector[SortedMap[Int, Double]]] = {
    val starting: Double = ins.sum

    ( ins, outs, prefs, Vector.empty[SortedMap[Int, Double]] ).tailRecM {
      case ( remIns, remOuts, remPrefs, acc ) =>
        val total: Double = remIns.sum
        if (total < Tolerance * starting)
          Right( acc ).pure[F].widen
        else if (remPrefs.isEmpty)
          Right(
            combine( acc, balanceNoPrefs( total, remIns, remOuts ) )
          ).pure[F].widen
        else
          solver
            .solve( remIns, remOuts, remPrefs.head.toVector )
            .map( tierSolution =>
              Left(
                (
                  tierSolution.remainingIns,
                  tierSolution.remainingOuts,
                  remPrefs.tail,
                  combine( acc, tierSolution.preferred )
                )
              )
            )
    }
  }

  private def combine(
      lhs: Vector[SortedMap[Int, Double]],
      rhs: Vector[SortedMap[Int, Double]]
  ): Vector[SortedMap[Int, Double]] =
    lhs.alignWith( rhs )( _.merge )

  private def balanceNoPrefs(
      total: Double,
      ins: Vector[Double],
      outs: Vector[Double]
  ): Vector[SortedMap[Int, Double]] = {

    AssignmentFlowBalancer
      .normalizedNoPrefs( ins.map( _ / total ), outs.map( _ / total ) )
      .nested
      .fmap( _ * total )
      .value
  }

}
