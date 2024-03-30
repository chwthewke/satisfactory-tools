package net.chwthewke.satisfactorytools
package prod
package planning

import cats.data.EitherT
import cats.data.NonEmptyVector
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.vector._
import org.scalacheck.Gen
import org.scalacheck.cats.implicits._
import org.scalatest.Assertion
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

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

  def genPrefs(
      tierCountGen: Gen[Int],
      tierSizeGen: Gen[Int]
  )( m: Int, n: Int ): Gen[Vector[NonEmptyVector[( Int, Int )]]] =
    for {
      tierCount <- tierCountGen.map( _.min( m * n ).max( 0 ) )
      tierSizes <- ( tierCount, n * m, List.empty[Int] ).tailRecM[Gen, List[Int]] {
                     case ( r, av, ts ) =>
                       if (r <= 0) Gen.const( Right( ts ) )
                       else {
                         tierSizeGen
                           .map( _.max( av - r + 1 ).min( 1 ) )
                           .map( t => Left( ( r - 1, av - t, t :: ts ) ) )
                       }
                   }
      selected <- Gen.pick( tierSizes.sum, 0.until( m ).flatMap( i => 0.until( n ).map( j => ( i, j ) ) ) )
    } yield {

      @tailrec
      def popPrefs(
          sizes: List[Int],
          assignments: Vector[( Int, Int )],
          acc: Vector[NonEmptyVector[( Int, Int )]]
      ): Vector[NonEmptyVector[( Int, Int )]] = {
        sizes match {
          case Nil => acc
          case s :: t =>
            popPrefs(
              t,
              assignments.drop( s ),
              acc.prependedAll( assignments.take( s ).toNev )
            )
        }
      }

      popPrefs( tierSizes, selected.toVector, Vector.empty )

    }

  def genAnyPrefs( m: Int, n: Int ): Gen[Vector[NonEmptyVector[( Int, Int )]]] =
    Gen.oneOf(
      genPrefs( Gen.choose( 1, 2 ), Gen.choose( 1, 2 ) )( m, n ),                   // few prefs
      genPrefs( Gen.choose( 1, n.min( m ) ), Gen.choose( 1, n.max( m ) ) )( m, n ), // many prefs
      Gen.const( Vector.empty )                                                     // no prefs
    )

  val genParams: Gen[( Vector[Double], Vector[Double], Vector[NonEmptyVector[( Int, Int )]] )] = for {
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

  def balancerProperties( balancer: AssignmentFlowBalancer[IO] ): Unit = {

    def testNormalizedNoPrefs(
        assertion: ( Vector[Double], Vector[Double] ) => Vector[SortedMap[Int, Double]] => Assertion
    ): Assertion =
      forAll( genParams ) {
        case ( ins, outs, prefs ) =>
          val square: Vector[SortedMap[Int, Double]] =
            balancer.balance( ins, outs, prefs ).unsafeRunSync()

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

  "the naive balancer" must {
    val solver: AssignmentSolver[IO] =
      ( ins, outs, prefs ) => IO.delay( AssignmentSolver.Naive.solve( ins, outs, prefs ) )

    behave like balancerProperties( new AssignmentFlowBalancer( solver ) )
  }

  "the optimized balancer" must {
    val solver: AssignmentSolver[IO] = ( ins, outs, prefs ) =>
      EitherT( IO.delay( AssignmentSolver.Optimized.solve( ins, outs, prefs ) ) )
        .leftMap( new RuntimeException( _ ) )
        .rethrowT

    behave like balancerProperties( new AssignmentFlowBalancer( solver ) )
  }

}
