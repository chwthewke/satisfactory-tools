package net.chwthewke.satisfactorytools
package data

import cats.Applicative
import cats.Eval
import cats.Monad
import cats.Order
import cats.Show
import cats.Traverse
import cats.syntax.show._
import scala.annotation.tailrec
import scala.collection.Factory

final case class Countable[N, A]( item: A, amount: N )

object Countable {
  implicit class GatherOps[F[x] <: Iterable[x], N, A]( private val self: F[Countable[N, A]] ) {
    def gather( implicit N: Numeric[N], F: Factory[Countable[N, A], F[Countable[N, A]]] ): F[Countable[N, A]] =
      self
        .groupMap( _.item )( _.amount )
        .map { case ( item, amounts ) => Countable( item, amounts.sum ) }
        .to( F )
  }

  implicit def countableMonad[N](
      implicit N: Numeric[N]
  ): Monad[Countable[N, *]] with Traverse[Countable[N, *]] = new CountableInstance[N]

  private class CountableInstance[N]( implicit N: Numeric[N] )
      extends Monad[Countable[N, *]]
      with Traverse[Countable[N, *]] {
    override def traverse[G[_], A, B]( fa: Countable[N, A] )( f: A => G[B] )(
        implicit G: Applicative[G]
    ): G[Countable[N, B]] = G.map( f( fa.item ) )( Countable( _, fa.amount ) )

    override def foldLeft[A, B]( fa: Countable[N, A], b: B )( f: ( B, A ) => B ): B =
      f( b, fa.item )

    override def foldRight[A, B]( fa: Countable[N, A], lb: Eval[B] )( f: ( A, Eval[B] ) => Eval[B] ): Eval[B] =
      f( fa.item, lb )

    override def flatMap[A, B]( fa: Countable[N, A] )( f: A => Countable[N, B] ): Countable[N, B] = {
      val Countable( b, m ) = f( fa.item )
      Countable( b, N.times( m, fa.amount ) )
    }

    override def tailRecM[A, B]( a: A )( f: A => Countable[N, Either[A, B]] ): Countable[N, B] = {
      @tailrec
      def loop( acc: N, a0: A ): Countable[N, B] =
        f( a0 ) match {
          case Countable( Left( a1 ), n )  => loop( N.times( n, acc ), a1 )
          case Countable( Right( a1 ), n ) => Countable( a1, N.times( n, acc ) )
        }

      loop( N.one, a )
    }

    override def pure[A]( x: A ): Countable[N, A] = Countable( x, N.one )
  }

  implicit def countableShow[N: Show, A: Show]: Show[Countable[N, A]] =
    Show.show { case Countable( name, amount ) => show"$name ($amount)" }

  implicit def countableOrder[N: Order, A: Order]: Order[Countable[N, A]] =
    Order.by { case Countable( item, amount ) => ( item, amount ) }
}
