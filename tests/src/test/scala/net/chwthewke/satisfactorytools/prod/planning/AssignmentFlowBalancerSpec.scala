package net.chwthewke.satisfactorytools
package prod
package planning

import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.traverse._
import org.scalacheck.Gen
import org.scalacheck.cats.implicits._
import org.scalatest.Assertion
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

import net.chwthewke.satisfactorytools.prod.ojsolver.OptimizedAssignmentSolver

class AssignmentFlowBalancerSpec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  import FlowBalancer.Tolerance

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration( minSuccessful = 1000 )

  ////////////////////
  // Generators

  val genInOuts: Gen[Vector[Double]] = for {
    n       <- Gen.sized( sz => Gen.choose( 1, 1.max( sz ).min( 8 ) ) )
    doubles <- Gen.containerOfN[Vector, Double]( n, Gen.choose( 1d, 1000d ) )
  } yield doubles

  val genNormalizedInOuts: Gen[Vector[Double]] =
    genInOuts.map { doubles =>
      val total: Double = doubles.sum
      doubles.map( _ / total )
    }

  def genPrefs( countGen: Int => Gen[Int] )( m: Int, n: Int ): Gen[SortedMap[Int, SortedSet[Int]]] =
    0.until( m )
      .toVector
      .traverse(
        i =>
          countGen( n )
            .flatMap( Gen.pick( _, 0.until( n ) ) )
            .map( js => ( i, js.to( SortedSet ) ) )
      )
      .map( _.to( SortedMap ) )

  def genAnyPrefs( m: Int, n: Int ): Gen[SortedMap[Int, SortedSet[Int]]] =
    Gen.oneOf(
      genPrefs( k => Gen.choose( 0, k.min( 2 ) ) )( m, n ), // few prefs
      genPrefs( k => Gen.choose( k / 2, k ) )( m, n ),      // many prefs
      Gen.const( SortedMap.empty[Int, SortedSet[Int]] )     // no prefs
    )

  val genParams: Gen[( Vector[Double], Vector[Double], SortedMap[Int, SortedSet[Int]] )] = for {
    ins   <- genInOuts
    outs  <- genNormalizedInOuts
    prefs <- genAnyPrefs( ins.size, outs.size )
  } yield {
    val total = ins.sum
    ( ins, outs.map( _ * total ), prefs )
  }

  ////////////////////
  // Assertions

  def verifySquareTotal( total: Double )( square: Vector[SortedMap[Int, Double]] ): Assertion = {

    square.foldMap( _.combineAll ) must ===( total +- Tolerance * total.max( 1d ) )

  }

  def verifySquareIns( ins: Vector[Double] )( square: Vector[SortedMap[Int, Double]] ): Assertion = {
    val actualIns: Vector[Double] = square.fmap( _.combineAll )

    actualIns.size must ===( ins.size )

    actualIns.zipWithIndex.foreach {
      case ( in, i ) =>
        in must ===( ins( i ) +- Tolerance * ins( i ).max( 1d ) )
    }

    succeed
  }

  def verifySquareOuts( outs: Vector[Double] )( square: Vector[SortedMap[Int, Double]] ): Assertion = {
    val actualOuts: SortedMap[Int, Double] = square.combineAll

    actualOuts.keySet must contain theSameElementsAs outs.indices

    actualOuts.foreach {
      case ( j, out ) =>
        out must ===( outs( j ) +- Tolerance * outs( j ).max( 1d ) )
    }

    succeed
  }

  "the normalized no-prefs balancer" must {

    def testNormalizedNoPrefs(
        assertion: ( Vector[Double], Vector[Double] ) => Vector[SortedMap[Int, Double]] => Assertion
    ): Assertion =
      forAll( genNormalizedInOuts, genNormalizedInOuts ) { ( ins, outs ) =>
        val square: Vector[SortedMap[Int, Double]] = AssignmentFlowBalancer.normalizedNoPrefs( ins, outs )

        assertion( ins, outs )( square )
      }

    "conserve the total" in {
      testNormalizedNoPrefs( ( _, _ ) => verifySquareTotal( 1d ) )
    }

    "conserve the ins" in {
      testNormalizedNoPrefs( ( ins, _ ) => verifySquareIns( ins ) )
    }

    "conserve the outs" in {
      testNormalizedNoPrefs( ( _, outs ) => verifySquareOuts( outs ) )
    }

  }

  def balancerProperties( balancer: AssignmentFlowBalancer ): Unit = {

    def testNormalizedNoPrefs(
        assertion: ( Vector[Double], Vector[Double] ) => Vector[SortedMap[Int, Double]] => Assertion
    ): Assertion =
      forAll( genParams ) {
        case ( ins, outs, prefs ) =>
          val square: Vector[SortedMap[Int, Double]] = balancer.balance( ins, outs, prefs )

          assertion( ins, outs )( square )
      }

    "conserve the total" in {
      testNormalizedNoPrefs( ( ins, _ ) => verifySquareTotal( ins.sum ) )
    }

    "conserve the ins" in {
      testNormalizedNoPrefs( ( ins, _ ) => verifySquareIns( ins ) )
    }

    "conserve the outs" in {
      testNormalizedNoPrefs( ( _, outs ) => verifySquareOuts( outs ) )
    }

  }

  "the general balancer" must {
    behave like balancerProperties( new AssignmentFlowBalancer( None ) )
  }

  "the optimized balancer" must {
    behave like balancerProperties( new AssignmentFlowBalancer( Some( OptimizedAssignmentSolver ) ) )
  }

}
