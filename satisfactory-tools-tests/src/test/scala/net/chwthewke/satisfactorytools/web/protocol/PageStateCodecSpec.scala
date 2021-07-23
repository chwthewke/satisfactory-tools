package net.chwthewke.satisfactorytools
package web.protocol

import cats.effect.unsafe.implicits._
import cats.syntax.functor._
import cats.syntax.traverse._
import fr.thomasdufour.autodiff.Diff
import fr.thomasdufour.autodiff.derived
import fr.thomasdufour.autodiff.extra.enumeratum._
import fr.thomasdufour.autodiff.extra.scalatest.AutodiffMatchers.~=
import org.scalacheck.Gen
import org.scalacheck.cats.implicits._
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.Inside
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import scala.concurrent.duration.FiniteDuration
import scodec.Attempt
import scodec.Codec
import scodec.DecodeResult
import scodec.bits.BitVector

import loader.Loader
import model.Model
import model.Recipe
import web.state.CustomGroupSelection
import web.state.InputTab
import web.state.OutputTab
import web.state.OutputTab.CustomGroup
import web.state.PageState

class PageStateCodecSpec
    extends AnyWordSpec
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with Inside
    with TypeCheckedTripleEquals {

  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration( minSuccessful = 100 )

  val model: Model = Loader.io.loadModel.unsafeRunSync()

  import prod.Generators.Loaded._

  def roundTrip[A]( codec: Codec[A], generator: Gen[A] ): Unit =
    "round trip" in {
      forAll( generator ) { value =>
        inside( codec.encode( value ) ) {
          case Attempt.Failure( cause ) => fail( "ENCODE FAILED " + cause.messageWithContext )
          case Attempt.Successful( bits ) =>
            inside( codec.decode( bits ) ) {
              case Attempt.Failure( cause ) => fail( "DECODE FAILED " + cause.messageWithContext )
              case Attempt.Successful( DecodeResult( result, remainder ) ) =>
                result should ===( value )
                remainder should ===( BitVector.empty )
            }
        }

      }
    }

  def roundTripD[A: Diff]( codec: Codec[A], generator: Gen[A] ): Unit =
    "round trip" in {
      forAll( generator ) { value =>
        inside( codec.encode( value ) ) {
          case Attempt.Failure( cause ) => fail( "ENCODE FAILED " + cause.messageWithContext )
          case Attempt.Successful( bits ) =>
            inside( codec.decode( bits ) ) {
              case Attempt.Failure( cause ) => fail( "DECODE FAILED " + cause.messageWithContext )
              case Attempt.Successful( DecodeResult( result, remainder ) ) =>
                result should ~=( value )
                remainder should ===( BitVector.empty )
            }
        }

      }
    }

  val genCustomGroupSelection: Gen[CustomGroupSelection] = for {
    groupCount <- Gen.choose( 0, 15 )
    selection  <- pick[Vector]( model.manufacturingRecipes )
    groups     <- selection.traverse( r => Gen.choose( 0, groupCount ).tupleLeft( r ) )
  } yield CustomGroupSelection( groupCount, groups.toMap.filter { case ( _, v ) => v != 0 } )

  private val outputTabStatic: Gen[OutputTab] =
    Gen.oneOf( Vector( OutputTab.BlocksTab, OutputTab.ResourcesTab, OutputTab.ResourcesTab ) )
  private def outputTabGroup( count: Int ): Gen[OutputTab] = Gen.choose( 1, count ).map( CustomGroup( _ ) )

  val genState: Gen[PageState] =
    for {
      inputs       <- genInputs
      inputTab     <- Gen.oneOf( InputTab.values )
      customGroups <- genCustomGroupSelection
      outputTab <- if (customGroups.count > 0) Gen.oneOf( outputTabStatic, outputTabGroup( customGroups.count ) )
                  else outputTabStatic
      factory <- Gen.option( Gen.oneOf( Gen.alphaNumStr.map( Left( _ ) ), genFactory.map( Right( _ ) ) ) )
    } yield PageState( inputs, inputTab, outputTab, factory, customGroups )

  import diff._

  "the map options codec" should {
    behave like roundTrip( Codecs.resoucesOptionsCodec( model ), genResourceOptions )
  }

  "the options codec" should {
    behave like roundTrip( Codecs.optionsCodec, genOptions )
  }

  "the recipe list codec" should {
    behave like roundTrip( Codecs.recipeListCodec( model ), genRecipeList )
  }

  "the bill data codec" should {
    behave like roundTrip( Codecs.billCodec( model ), genBill )
  }

  "the inputs codec" should {
    behave like roundTrip( Codecs.inputsCodec( model ), genInputs )
  }

  "the factory codec" should {
    behave like roundTrip( Codecs.factoryCodec( model ), genFactory )
  }

  "the state codec" should {
    behave like roundTripD( PageState.pageStateCodec( model ), genState )
  }

  object diff {
    implicit val finiteDurationDiff: Diff[FiniteDuration] = Diff.defaultEqShow[FiniteDuration]

    implicit val outputTabDiff: Diff[OutputTab] = Diff.defaultEqShow[OutputTab]

    implicit val customGroupSelectionDiff: Diff[CustomGroupSelection] = {
      import derived.auto._

      implicit val mapDiff: Diff[Map[Recipe, Int]] =
        Diff
          .mapDiff[Recipe, Int]
          .contramap[Map[Recipe, Int]]( _.filter { case ( _, v ) => v != 0 } )

      derived.semi.diff[CustomGroupSelection]
    }

    implicit val pageStateDiff: Diff[PageState] = {
      import derived.auto._

      derived.semi.diff[PageState]
    }

  }

}
