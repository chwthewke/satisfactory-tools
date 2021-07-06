package net.chwthewke.satisfactorytools
package web.protocol

import alleycats.std.map._
import cats.Traverse
import cats.effect.unsafe.implicits._
import cats.syntax.apply._
import cats.syntax.functor._
import cats.syntax.traverse._
import fr.thomasdufour.autodiff.Diff
import fr.thomasdufour.autodiff.derived
import fr.thomasdufour.autodiff.extra.enumeratum._
import fr.thomasdufour.autodiff.extra.scalatest.AutodiffMatchers.~=
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.cats.implicits._
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.Inside
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import scala.annotation.nowarn
import scala.collection.{Factory => CBF}
import scala.concurrent.duration.FiniteDuration
import scodec.Attempt
import scodec.Codec
import scodec.DecodeResult
import scodec.bits.BitVector

import data.Countable
import data.Item
import loader.Loader
import model.Bill
import model.ExtractorType
import model.Model
import model.Options
import model.Recipe
import model.RecipeList
import model.ResourceDistrib
import model.ResourceOptions
import model.ResourceWeights
import prod.ClockedRecipe
import prod.Factory
import prod.SolverInputs
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

  def pick[CC[x] <: Iterable[x]] = new PickPartiallyApplied[CC]()

  class PickPartiallyApplied[CC[x] <: Iterable[x]]() {
    def apply[A]( src: Seq[A] )( implicit F: CBF[A, CC[A]] ): Gen[CC[A]] =
      Gen.choose( 0, src.size ).flatMap( Gen.pick( _, src ) ).map( _.to( F ) )
  }

  def genCountables[F[_]: Traverse, A]( things: Gen[F[A]] ): Gen[F[Countable[Double, A]]] =
    things.flatMap( _.traverse( thing => arbitrary[Float].map( am => Countable( thing, am.toDouble ) ) ) )

  val genResourceNodes: Gen[Map[ExtractorType, Map[Item, ResourceDistrib]]] =
    model.defaultResourceOptions.resourceNodes
      .traverse( _.traverse {
        case ResourceDistrib( impureNodes, normalNodes, pureNodes ) =>
          ( Gen.choose( 0, impureNodes ), Gen.choose( 0, normalNodes ), Gen.choose( 0, pureNodes ) )
            .mapN( ResourceDistrib( _, _, _ ) )
      } )

  val genResourceWeights: Gen[ResourceWeights] =
    model.extractedItems
      .traverse( it => Gen.choose( 0, 2 * ResourceWeights.range ).tupleLeft( it ) )
      .map( v => ResourceWeights( v.filter( _._2 != ResourceWeights.range ).toMap ) )

  val genResourceOptions: Gen[ResourceOptions] =
    ( genResourceNodes, genResourceWeights )
      .mapN( ResourceOptions( _, _ ) )

  val genOptions: Gen[Options] =
    (
      Gen.oneOf( Options.Belt.values ),
      Gen.oneOf( Options.Pipe.values ),
      Gen.oneOf( Options.Miner.values ),
      Gen.oneOf( Options.ClockSpeed.values ),
      pick[Set]( ExtractorType.values ),
      pick[Set]( Seq[ExtractorType]( ExtractorType.OilPump, ExtractorType.WaterPump ) )
    ).mapN( Options( _, _, _, _, _, _ ) )

  val genRecipeList: Gen[RecipeList] =
    pick[Vector]( model.manufacturingRecipes )
      .map( _.sortBy( model.manufacturingRecipes.indexOf ) )
      .map( RecipeList( _ ) )

  val genBill: Gen[Bill] =
    genCountables( Gen.choose( 0, 40 ).flatMap( Gen.pick( _, model.items.values.toSeq ) ).map( _.toVector ) )
      .map( Bill( _ ) )

  val genInputs: Gen[SolverInputs] =
    ( genBill, genRecipeList, genOptions, genResourceOptions )
      .mapN( SolverInputs( _, _, _, _ ) )

  def genClockedRecipes( recipes: Vector[Recipe] ): Gen[Vector[ClockedRecipe]] =
    pick[Vector]( recipes )
      .flatMap(
        _.traverse(
          recipe =>
            ( arbitrary[Short], arbitrary[Float] )
              .mapN( ( am, bc ) => ClockedRecipe.overclocked( Countable( recipe, am & 0xFFFF ), bc.toDouble ) )
        )
      )

  def genManufacturingRecipes(
      recipes: Vector[Recipe]
  ): Gen[Vector[Countable[Double, Recipe]]] =
    pick[Vector]( recipes )
      .flatMap(
        _.traverse(
          recipe =>
            arbitrary[Float]
              .map( am => Countable( recipe, am.toDouble ) )
        )
      )

  val genFactory: Gen[Factory] =
    for {
      extractionBlocks    <- genClockedRecipes( model.extractionRecipes.map( _._3 ) )
      manufacturingBlocks <- genManufacturingRecipes( model.manufacturingRecipes )
      itemsIn             <- pick[Vector]( model.items.values.toVector )
      itemsInCountable    <- genCountables( itemsIn )
      itemsOut            <- pick[Vector]( model.items.values.toVector )
      itemsOutCountable   <- genCountables( itemsOut )
    } yield Factory( extractionBlocks, manufacturingBlocks, itemsInCountable, itemsOutCountable )

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
        (Diff
          .mapDiff[Recipe, Int]: @nowarn( "cat=lint-byname-implicit" ))
          .contramap[Map[Recipe, Int]]( _.filter { case ( _, v ) => v != 0 } )

      derived.semi.diff[CustomGroupSelection]: @nowarn( "cat=lint-byname-implicit" )
    }

    implicit val pageStateDiff: Diff[PageState] = {
      import derived.auto._

      derived.semi.diff[PageState]: @nowarn( "cat=lint-byname-implicit" )
    }

  }

}
