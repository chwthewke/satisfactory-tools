package net.chwthewke.satisfactorytools
package prod
package planning

import cats.data.NonEmptyVector
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

trait FlowBalancer {

  /**
    *
    * @param ins   parts of production, all > 0
    * @param outs  parts of consumption, all > 0, also `outs.sum == ins.sum`
    * @param prefs preferred assignments of producers to consumers, e.g. `Map(0 -> BitSet(1, 2))` assigns producer 0 to
    *              consumers 1 and 2.
    * @return a vector `square` of assignments of ins' values to outs, s.t. (within Double error tolerance)
    *            - `square.map( _.fold ) == ins`
    *            - `outs.indices.map(j => square.fold.getOrElse(j, 0d)) == outs`
    */
  def balance(
      ins: Vector[Double],
      outs: Vector[Double],
      prefs: SortedMap[Int, SortedSet[Int]]
  ): Vector[SortedMap[Int, Double]]

  def balance(
      ins: Vector[Double],
      outs: Vector[Double],
      prefs: Vector[NonEmptyVector[( Int, Int )]]
  ): Vector[SortedMap[Int, Double]]

}

object FlowBalancer {
  val Tolerance: Double = 1e-8
}
