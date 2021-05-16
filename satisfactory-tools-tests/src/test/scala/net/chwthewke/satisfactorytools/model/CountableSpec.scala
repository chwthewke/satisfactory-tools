package net.chwthewke.satisfactorytools
package model

import cats.Eq
import cats.data.Const
import cats.data.ZipLazyList
import cats.derived.semiauto
import cats.kernel.CommutativeMonoid
import cats.laws.discipline.MonadTests
import cats.laws.discipline.TraverseTests
import cats.syntax.apply._
import org.scalacheck.Arbitrary
import org.scalacheck.cats.implicits._
import org.scalatest.prop.Configuration
import org.scalatest.wordspec.AnyWordSpec
import org.typelevel.discipline.scalatest.WordSpecDiscipline

import data.Countable

class CountableSpec extends AnyWordSpec with WordSpecDiscipline with Configuration {
  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration( minSuccessful = 100 )

  implicit def arbitraryCountable[N, A]( implicit arbN: Arbitrary[N], arbA: Arbitrary[A] ): Arbitrary[Countable[N, A]] =
    Arbitrary( ( arbA.arbitrary, arbN.arbitrary ).mapN( Countable( _, _ ) ) )

  implicit def countableEq[N: Eq, A: Eq]: Eq[Countable[N, A]] = semiauto.eq[Countable[N, A]]

  implicit def arbZipLazyList[A]( implicit arbA: Arbitrary[LazyList[A]] ): Arbitrary[ZipLazyList[A]] =
    Arbitrary( arbA.arbitrary.map( ZipLazyList( _ ) ) )

  implicit def arbConst[A, B]( implicit arbA: Arbitrary[A] ): Arbitrary[Const[A, B]] =
    Arbitrary( arbA.arbitrary.map( Const( _ ) ) )

  implicit val intCommutativeMonoid: CommutativeMonoid[Int] = CommutativeMonoid.instance( 0, _ + _ )

  "Countable[Int, *]" should {
    checkAll( "monad laws", MonadTests[Countable[Int, *]].monad[Int, String, Char] )

    checkAll(
      "traverse laws",
      TraverseTests[Countable[Int, *]].traverse[Int, String, Char, Int, ZipLazyList, Const[Int, *]]
    )

  }

}
