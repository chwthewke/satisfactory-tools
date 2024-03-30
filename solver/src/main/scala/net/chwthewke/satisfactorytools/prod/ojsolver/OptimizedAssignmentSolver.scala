package net.chwthewke.satisfactorytools
package prod
package ojsolver

import cats.syntax.functor._
import cats.syntax.show._
import org.ojalgo.optimisation.Expression
import org.ojalgo.optimisation.ExpressionsBasedModel
import org.ojalgo.optimisation.Optimisation
import org.ojalgo.optimisation.Variable
import scala.collection.immutable.SortedMap

import prod.planning.AssignmentSolution
import prod.planning.AssignmentSolver

trait OptimizedAssignmentSolver extends AssignmentSolver[Either[String, *]] {

  private def edgeVarName( i: Int, j: Int ): String = show"E_${i}_$j"
  private def inExprName( i: Int ): String          = show"I_$i"
  private def outExprName( j: Int ): String         = show"O_$j"

  def solve(
      ins: Vector[Double],
      outs: Vector[Double],
      prefs: Vector[( Int, Int )]
  ): Either[String, AssignmentSolution] = {

    val model: ExpressionsBasedModel = new ExpressionsBasedModel()

    // E_i_j >= 0
    val edgeVars: Map[( Int, Int ), Variable] =
      prefs.fproduct {
        case ( i, j ) =>
          model.addVariable( edgeVarName( i, j ) ).weight( 1d ).lower( 0d )
      }.toMap

    // I_i >= \sum E_i_j for (i, j) in prefs
    ins.zipWithIndex.foreach {
      case ( in, i ) =>
        val expr: Expression = model.addExpression( inExprName( i ) ).upper( in )

        outs.indices.map( ( i, _ ) ).flatMap( edgeVars.get ).foreach( e => expr.set( e, 1d ) )
    }

    // O_j >= \sum E_i_j for (i, j) in prefs
    outs.zipWithIndex.foreach {
      case ( out, j ) =>
        val expr: Expression = model.addExpression( outExprName( j ) ).upper( out )

        ins.indices.map( ( _, j ) ).flatMap( edgeVars.get ).foreach( e => expr.set( e, 1d ) )
    }

    val result: Optimisation.Result = model.maximise()

    Option
      .when( result.getState.isSuccess ) {
        val edgeValues: SortedMap[( Int, Int ), Double] =
          SortedMap.from( prefs.iterator.map {
            case ( i, j ) => ( ( i, j ), edgeVars( ( i, j ) ).getValue.doubleValue() )
          } )

        val ( remIns: Vector[Double], remOuts: Vector[Double] ) =
          edgeValues.foldLeft( ( ins, outs ) ) {
            case ( ( insAcc, outsAcc ), ( ( i, j ), amt ) ) =>
              ( insAcc.updated( i, insAcc( i ) - amt ), outsAcc.updated( j, outsAcc( j ) - amt ) )
          }

        AssignmentSolution( remIns, remOuts, edgeValues )
      }
      .toRight( s"Solver state: ${result.getState}" )
  }
}
