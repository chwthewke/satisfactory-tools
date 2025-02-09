package net.chwthewke.satisfactorytools
package data

import cats.syntax.all._
import fr.thomasdufour.autodiff.Diff
import fr.thomasdufour.autodiff.extra.enumeratum._
import fr.thomasdufour.autodiff.derived

trait DiffInstances {

  val Tolerance = 1e-9

  private def doubleEq( x: Double, y: Double ) = {
    val mag = x.abs.max( y.abs )

    ( x - y ).abs < Tolerance * mag || mag * Tolerance < Double.MinPositiveValue
  }

  implicit val doubleDiff: Diff[Double] =
    Diff.explicitEqShow( doubleEq, _.show )

  implicit val classNameDiff: Diff[ClassName] = Diff[String].contramap( _.name )

  implicit val itemDiff: Diff[Item] = {
    import derived.auto._

    derived.semi.diff[Item]
  }

  implicit def countableDiff[N: Diff, A: Diff]: Diff[Countable[N, A]] = {
    derived.semi.diff[Countable[N, A]]
  }

}

object diff extends DiffInstances
