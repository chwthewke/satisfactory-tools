package net.chwthewke.satisfactorytools
package data

import cats.Traverse
import cats.syntax.all._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.cats.implicits._

trait Generators extends _root_.net.chwthewke.satisfactorytools.Generators {

  def genCountables[F[_]: Traverse, A]( things: Gen[F[A]] ): Gen[F[Countable[Double, A]]] =
    things.flatMap( _.traverse( thing => arbitrary[Float].map( am => Countable( thing, am.toDouble ) ) ) )

}

object Generators extends Generators
