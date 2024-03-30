package net.chwthewke.satisfactorytools
package prod.planning

import cats.Id
import cats.data.EitherT
import cats.effect.kernel.Sync
import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

import prod.ojsolver.OptimizedAssignmentSolver

trait AssignmentSolver[F[_]] { self =>
  def solve(
      ins: Vector[Double],
      outs: Vector[Double],
      prefs: Vector[( Int, Int )]
  ): F[AssignmentSolution]
}

object AssignmentSolver {
  class Combined[F[_]]( implicit val F: Sync[F] ) extends AssignmentSolver[F] {
    override def solve(
        ins: Vector[Double],
        outs: Vector[Double],
        prefs: Vector[( Int, Int )]
    ): F[AssignmentSolution] =
      EitherT( F.interruptible( Optimized.solve( ins, outs, prefs ) ) )
        .foldF( _ => F.interruptible( Naive.solve( ins, outs, prefs ) ), F.pure )
  }

  private[planning] object Naive extends AssignmentSolver[Id] {

    import FlowBalancer.Tolerance

    override def solve(
        ins: Vector[Double],
        outs: Vector[Double],
        prefs: Vector[( Int, Int )]
    ): AssignmentSolution =
      selectPreferred( ins, outs, prefs.toSet, SortedMap.empty )

    @tailrec
    private def selectPreferred(
        ins: Vector[Double],
        outs: Vector[Double],
        prefs: Set[( Int, Int )],
        acc: SortedMap[( Int, Int ), Double]
    ): AssignmentSolution = {
      // NOTE this has bias and is possibly non optimal I guess
      //      we have a linear programming implementation... and can use this as a fallback
      val selection: Option[( Int, Int, Double )] =
        prefs
          .map { case ( i, j ) => ( i, j, ins( i ) min outs( j ) ) }
          .maxByOption( _._3 )
          .filter( _._3 > Tolerance )

      selection match {
        case Some( ( i, j, amt ) ) =>
          selectPreferred(
            ins.updated( i, ins( i ) - amt ),
            outs.updated( j, outs( j ) - amt ),
            prefs - ( ( i, j ) ),
            acc + ( ( i, j ) -> amt )
          )

        case None => AssignmentSolution( ins, outs, acc )
      }
    }
  }

  private[planning] object Optimized extends OptimizedAssignmentSolver
}
