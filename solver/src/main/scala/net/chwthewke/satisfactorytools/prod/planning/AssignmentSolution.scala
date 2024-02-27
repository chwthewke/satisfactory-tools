package net.chwthewke.satisfactorytools
package prod
package planning

import scala.collection.immutable.SortedMap

case class AssignmentSolution(
    remainingIns: Vector[Double],
    remainingOuts: Vector[Double],
    assignments: SortedMap[( Int, Int ), Double]
) {
  def preferred: Vector[SortedMap[Int, Double]] =
    remainingIns.indices
      .map( i => assignments.collect { case ( ( `i`, j ), amt ) => ( j, amt ) } )
      .toVector

}
