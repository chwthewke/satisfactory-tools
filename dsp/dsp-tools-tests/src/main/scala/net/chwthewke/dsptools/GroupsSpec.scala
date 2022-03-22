package net.chwthewke.dsptools

import cats.syntax.apply._
import cats.syntax.flatMap._
import org.scalacheck.Gen
import org.scalacheck.Shrink
import org.scalacheck.cats.implicits._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class GroupsSpec extends AnyWordSpec with Matchers with ScalaCheckPropertyChecks {

  import Shrink.shrinkAny
  import Groups.Search

  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration( minSuccessful = 1000 )

  val search: Search[Gen] = new Search( 5 ) {
    override def permutation( groupCount: Int ): Gen[( ( Int, Int ), ( Int, Int ) )] =
      (
        ( Gen.choose( 0, groupCount - 1 ), Gen.choose( 0, groupCount - 1 ) ).tupled,
        ( Gen.choose( 0, size - 1 ), Gen.choose( 0, size - 1 ) ).tupled
      ).tupled
  }

  val vector: Gen[Vector[Vector[Int]]] =
    Gen.choose( 4, 21 ).map( n => 0.until( n ).toVector.grouped( 5 ).toVector )

  val groupsAndPermutation: Gen[( Vector[Vector[Int]], ( ( Int, Int ), ( Int, Int ) ) )] =
    vector.mproduct( c => search.permutation( c.size ) )

  val inputsAndResult: Gen[( Vector[Vector[Int]], ( ( Int, Int ), ( Int, Int ) ), Vector[Vector[Int]] )] =
    for { ( groups, p @ ( pg, pe ) ) <- groupsAndPermutation } yield (
      groups,
      p,
      search.applyPermutation( groups, pg, pe )
    )

  "applying a permutation" should {
    "preserve the elements" in {
      forAll( inputsAndResult ) {
        case ( groups, _, result ) =>
          result.flatten must contain theSameElementsAs groups.flatten
      }
    }
  }
}
