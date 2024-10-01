package net.chwthewke.satisfactorytools
package protocol

import cats.Applicative
import cats.Apply
import cats.Eval
import cats.Show
import cats.Traverse
import cats.derived.semiauto
import cats.syntax.applicative._
import cats.syntax.functor._

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

  def groupCount: Int = fold( 0, _ => 0, ( _, c, _ ) => c )
}

object SolutionHeader {
  val DefaultCustomGroups = 4
  val MaxCustomGroups     = 16

  final case object NotComputed                                              extends SolutionHeader[Nothing]
  final case class PlanError( error: String )                                extends SolutionHeader[Nothing]
  final case class Computed[T]( solutionId: T, groups: Int, lastGroup: Int ) extends SolutionHeader[T]

  implicit def solutionHeaderShow[T: Show]: Show[SolutionHeader[T]] = semiauto.show[SolutionHeader[T]]

  // implicit val solutionHeaderTraverse: Traverse[SolutionHeader] = semiauto.traverse[SolutionHeader]

  implicit val solutionHeaderInstance: Traverse[SolutionHeader] with Apply[SolutionHeader] =
    new Traverse[SolutionHeader] with Apply[SolutionHeader] {
      override def traverse[G[_]: Applicative, A, B]( fa: SolutionHeader[A] )( f: A => G[B] ): G[SolutionHeader[B]] =
        fa match {
          case NotComputed                               => NotComputed.pure[G].widen
          case PlanError( error )                        => PlanError( error ).pure[G].widen
          case Computed( solutionId, groups, lastGroup ) => f( solutionId ).map( Computed( _, groups, lastGroup ) )
        }

      override def ap[A, B]( ff: SolutionHeader[A => B] )( fa: SolutionHeader[A] ): SolutionHeader[B] = ff match {
        case NotComputed        => NotComputed
        case PlanError( error ) => PlanError( error )
        case Computed( f, groups1, lastGroup1 ) =>
          fa match {
            case NotComputed        => NotComputed
            case PlanError( error ) => PlanError( error )
            case Computed( solutionId, groups2, lastGroup2 ) =>
              Computed( f( solutionId ), groups1.max( groups2 ), lastGroup1.max( lastGroup2 ) )
          }
      }

      override def foldLeft[A, B]( fa: SolutionHeader[A], b: B )( f: ( B, A ) => B ): B =
        fa match {
          case NotComputed                  => b
          case PlanError( _ )               => b
          case Computed( solutionId, _, _ ) => f( b, solutionId )
        }

      override def foldRight[A, B]( fa: SolutionHeader[A], lb: Eval[B] )( f: ( A, Eval[B] ) => Eval[B] ): Eval[B] =
        fa match {
          case NotComputed                  => lb
          case PlanError( _ )               => lb
          case Computed( solutionId, _, _ ) => f( solutionId, lb )
        }
    }
}
