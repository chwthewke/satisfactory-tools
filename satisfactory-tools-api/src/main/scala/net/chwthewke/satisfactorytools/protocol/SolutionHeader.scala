package net.chwthewke.satisfactorytools
package protocol

import cats.Show
import cats.Traverse
import cats.derived.semiauto

sealed trait SolutionHeader[+T] extends Product {
  import SolutionHeader._

  private def fold[A]( notComputed: => A, planError: String => A, present: ( T, Int, Int ) => A ): A =
    this match {
      case NotComputed                                   => notComputed
      case PlanError( error )                            => planError( error )
      case Computed( solutionId, groupCount, lastGroup ) => present( solutionId, groupCount, lastGroup )
    }

  def isComputed: Boolean = fold( false, _ => false, ( _, _, _ ) => true )

  def value: Option[T]                  = fold( None, _ => None, ( v, _, _ ) => Some( v ) )
  def valueAndCount: Option[( T, Int )] = fold( None, _ => None, ( v, c, _ ) => Some( ( v, c ) ) )

  def canAddGroup: Boolean    = fold( false, _ => false, ( _, c, _ ) => c < MaxCustomGroups )
  def canRemoveGroup: Boolean = fold( false, _ => false, ( _, c, l ) => c > l )

  def groupCount: Int = fold( DefaultCustomGroups, _ => DefaultCustomGroups, ( _, c, _ ) => c )
}

object SolutionHeader {
  val DefaultCustomGroups = 4
  val MaxCustomGroups     = 16

  final case object NotComputed                                              extends SolutionHeader[Nothing]
  final case class PlanError( error: String )                                extends SolutionHeader[Nothing]
  final case class Computed[T]( solutionId: T, groups: Int, lastGroup: Int ) extends SolutionHeader[T]

  implicit def solutionHeaderShow[T: Show]: Show[SolutionHeader[T]] = semiauto.show[SolutionHeader[T]]

  implicit val solutionHeaderTraverse: Traverse[SolutionHeader] = semiauto.traverse[SolutionHeader]
}
