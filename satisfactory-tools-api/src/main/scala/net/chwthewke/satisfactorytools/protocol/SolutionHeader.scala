package net.chwthewke.satisfactorytools
package protocol

import cats.Show
import cats.derived.semiauto

sealed trait SolutionHeader extends Product

object SolutionHeader {
  final case object NotComputed                                                        extends SolutionHeader
  final case class PlanError( error: String )                                          extends SolutionHeader
  final case class Computed( solutionId: SolutionId, groupCount: Int, lastGroup: Int ) extends SolutionHeader

  implicit val solutionHeaderShow: Show[SolutionHeader] = semiauto.show[SolutionHeader]
}
