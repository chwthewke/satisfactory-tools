package net.chwthewke.satisfactorytools
package prod
package planning

import cats.Eval
import cats.data.NonEmptyVector
import cats.syntax.align._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.functorFilter._
import cats.syntax.nested._
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

object AssignmentFlowBalancer {

  def normalizedNoPrefs( ins: Vector[Double], outs: Vector[Double] ): Vector[SortedMap[Int, Double]] =
    ins.map(
      in =>
        outs.zipWithIndex
          .mapFilter {
            case ( out, j ) =>
              val a: Double = in * out
              Option.when( a.abs > 0 )( ( j, a ) )
          }
          .to( SortedMap )
    )

}

class AssignmentFlowBalancer(
    val optimizedSolver: Option[AssignmentSolver[Either[String, *]]]
) extends FlowBalancer {
  import FlowBalancer.Tolerance

  /**
    *
    * @param ins parts of production, all > 0
    * @param outs parts of consumption, all > 0, also `outs.sum == ins.sum`
    * @param prefs preferred assignments of producers to consumers, e.g. `Map(0 -> BitSet(1, 2))` assigns producer 0 to
    *              consumers 1 and 2.
    * @return a vector `square` of assignments of ins' values to outs, s.t. (within Double error tolerance)
    *            - `square.map( _.fold ) == ins`
    *            - `outs.indices.map(j => square.fold.getOrElse(j, 0d)) == outs`
    */
  override def balance(
      ins: Vector[Double],
      outs: Vector[Double],
      prefs: SortedMap[Int, SortedSet[Int]]
  ): Vector[SortedMap[Int, Double]] = {

    val solution @ AssignmentSolution( remIns, remOuts, _ ) =
      if (prefs.forall( _._2.isEmpty ))
        AssignmentSolution( ins, outs, SortedMap.empty )
      else
        solvePreferredAssignments(
          ins,
          outs,
          Vector.from( prefs.iterator.flatMap { case ( i, js ) => js.map( j => ( i, j ) ) } )
        )

    val total: Double = remOuts.sum
    if (remOuts.sum < Tolerance * outs.sum)
      solution.preferred
    else {
      combine(
        AssignmentFlowBalancer
          .normalizedNoPrefs( remIns.map( _ / total ), remOuts.map( _ / total ) )
          .nested
          .fmap( _ * total )
          .value,
        solution.preferred
      )
    }
  }

  private def combine(
      lhs: Vector[SortedMap[Int, Double]],
      rhs: Vector[SortedMap[Int, Double]]
  ): Vector[SortedMap[Int, Double]] =
    lhs.alignWith( rhs )( _.merge )

  def balance(
      ins: Vector[Double],
      outs: Vector[Double],
      prefs: Vector[NonEmptyVector[( Int, Int )]]
  ): Vector[SortedMap[Int, Double]] =
    ( ins, outs, prefs, Vector.empty[SortedMap[Int, Double]] ).tailRecM {
      case ( remIns, remOuts, remPrefs, acc ) =>
        val total: Double = remIns.sum
        if (total < Tolerance * ins.sum)
          Eval.now( Right( acc ) )
        else if (prefs.isEmpty)
          Eval.now(
            Left(
              ins.as( 0d ),
              outs.as( 0d ),
              remPrefs,
              combine( acc, balanceNoPrefs( total, ins, outs ) )
            )
          )
        else
          Eval.later {
            val tierSolution: AssignmentSolution =
              solvePreferredAssignments( remIns, remOuts, remPrefs.head.toVector )
            Left(
              tierSolution.remainingIns,
              tierSolution.remainingOuts,
              prefs.tail,
              combine( acc, tierSolution.preferred )
            )
          }
    }.value

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

  private def solvePreferredAssignments(
      ins: Vector[Double],
      outs: Vector[Double],
      prefs: Vector[( Int, Int )]
  ): AssignmentSolution = {
    optimizedSolver
      .flatMap(
        solver =>
          solver.solve( ins, outs, prefs ) match {
            case Left( err ) =>
              println( s"Optimized assignment solver failed: $err" )
              None
            case Right( solution ) =>
              Some( solution )
          }
      )
      .getOrElse( AssignmentSolver.solve( ins, outs, prefs ) )
  }

}
