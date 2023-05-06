package net.chwthewke.satisfactorytools
package prod

import cats.Monad
import cats.Traverse
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.traverse._

package object adv {
  implicit class MoreTraverseOps[F[_], A]( private val self: F[A] ) {
    def updateWhere( p: A => Boolean )( f: A => A )( implicit F: Traverse[F] ): Option[F[A]] = {
      val ( modified, result ) =
        self.traverse( a => if (p( a )) ( ().some, f( a ) ) else ( none, a ) )
      modified.as( result )
    }

    def flatUpdateWhere( p: A => Boolean )( f: A => F[A] )( implicit F: Traverse[F], M: Monad[F] ): Option[F[A]] = {
      val ( modified, result ) =
        self.flatTraverse( a => if (p( a )) ( ().some, f( a ) ) else ( none, M.pure( a ) ) )
      modified.as( result )
    }

  }

}
