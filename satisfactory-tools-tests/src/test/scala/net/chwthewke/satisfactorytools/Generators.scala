package net.chwthewke.satisfactorytools

import org.scalacheck.Gen
import scala.annotation.tailrec
import scala.collection.{Factory => CBF}
import scala.language.implicitConversions

trait Generators {
  def pick[CC[x] <: Iterable[x]] = new PickPartiallyApplied[CC]()

  class PickPartiallyApplied[CC[x] <: Iterable[x]]() {
    def apply[A]( src: Seq[A] )( implicit F: CBF[A, CC[A]] ): Gen[CC[A]] =
      Gen.choose( 0, src.size ).flatMap( Gen.pick( _, src ) ).map( _.to( F ) )
  }

  class GenOps[A]( private val self: Gen[A] ) {
    val maxRetries = 100

    def one: A = {
      @tailrec
      def loop( r: Int ): A =
        if (r <= 0)
          throw new IllegalStateException( s"Unable to sample after $maxRetries tries" )
        else
          self.sample match {
            case Some( a ) => a
            case None      => loop( r - 1 )
          }

      loop( maxRetries )
    }
  }

  implicit def toGenOps[A]( gen: Gen[A] ): GenOps[A] = new GenOps( gen )

}

object Generators extends Generators
