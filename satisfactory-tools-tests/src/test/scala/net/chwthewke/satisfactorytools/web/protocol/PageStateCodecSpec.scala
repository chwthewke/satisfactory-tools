package net.chwthewke.satisfactorytools
package web.protocol

import alleycats.std.map._
import cats.Traverse
import cats.effect.unsafe.implicits._
import cats.syntax.apply._
import cats.syntax.functor._
import cats.syntax.traverse._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.cats.implicits._
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.Inside
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import scala.collection.{Factory => CBF}
import scodec.Attempt
import scodec.Codec
import scodec.DecodeResult
import scodec.bits.BitVector

import data.Loader
import model.Bill
import model.Countable
import model.Item
import model.Machine
import model.MapOptions
import model.Model
import model.Options
import model.Recipe
import model.RecipeList
import model.ResourceDistrib
import model.SolverInputs
import prod.Factory
import prod.FactoryBlock
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

  def pick[CC[x] <: Iterable[x]] = new PickPartiallyApplied[CC]()

  class PickPartiallyApplied[CC[x] <: Iterable[x]]() {
    def apply[A]( src: Seq[A] )( implicit F: CBF[A, CC[A]] ): Gen[CC[A]] =
      Gen.choose( 0, src.size ).flatMap( Gen.pick( _, src ) ).map( _.to( F ) )
  }

  def genCountables[F[_]: Traverse, A]( things: Gen[F[A]] ): Gen[F[Countable[Double, A]]] =
    things.flatMap( _.traverse( thing => arbitrary[Float].map( am => Countable( thing, am.toDouble ) ) ) )

  val genMapOptions: Gen[MapOptions] =
    model.defaultMapOptions.resourceNodes
      .traverse( _.traverse {
        case ResourceDistrib( impureNodes, normalNodes, pureNodes ) =>
          ( Gen.choose( 0, impureNodes ), Gen.choose( 0, normalNodes ), Gen.choose( 0, pureNodes ) )
            .mapN( ResourceDistrib( _, _, _ ) )
      } )
      .map( MapOptions( _ ) )

  val genOptions: Gen[Options] =
    (
      Gen.oneOf( Options.Belt.values ),
      Gen.oneOf( Options.Pipe.values ),
      Gen.oneOf( Options.Miner.values ),
      Gen.oneOf( Options.ClockSpeed.values ),
      pick[Set]( Options.Extractors.values ),
      pick[Set]( Seq[Options.Extractors]( Options.Extractors.OilExtractor, Options.Extractors.WaterExtractor ) )
    ).mapN( Options( _, _, _, _, _, _ ) )

  val genRecipeList: Gen[RecipeList] =
    pick[Vector]( model.manufacturingRecipes )
      .map( _.sortBy( model.manufacturingRecipes.indexOf ) )
      .map( RecipeList( _ ) )

  val genBill: Gen[Bill] =
    genCountables( Gen.choose( 0, 40 ).flatMap( Gen.pick( _, model.items.values.toSeq ) ).map( _.toVector ) )
      .map( Bill( _ ) )

  val genInputs: Gen[SolverInputs] =
    ( genBill, genRecipeList, genOptions, genMapOptions )
      .mapN( SolverInputs( _, _, _, _ ) )

  def genFactoryBlocks( recipes: Vector[Recipe[Machine, Item]] ): Gen[Vector[FactoryBlock]] =
    pick[Vector]( recipes )
      .flatMap(
        _.traverse(
          recipe =>
            ( arbitrary[Float], arbitrary[Float] )
              .mapN( ( am, bc ) => FactoryBlock( Countable( recipe, am.toDouble ), bc.toDouble ) )
        )
      )

  val genFactory: Gen[Factory] =
    for {
      extractionBlocks    <- genFactoryBlocks( model.extractionRecipes.map( _._3 ) )
      manufacturingBlocks <- genFactoryBlocks( model.manufacturingRecipes )
      itemsIn             <- pick[Vector]( model.items.values.toVector )
      itemsInCountable    <- genCountables( itemsIn )
      itemsOut            <- pick[Vector]( model.items.values.toVector )
      itemsOutCountable   <- genCountables( itemsOut )
    } yield Factory( extractionBlocks, manufacturingBlocks, itemsInCountable, itemsOutCountable )

  val genCustomGroupSelection: Gen[CustomGroupSelection] =
    pick[Vector]( model.manufacturingRecipes )
      .flatMap( _.traverse( r => Gen.choose( 0, 5 ).tupleLeft( r ) ) )
      .map( v => CustomGroupSelection( v.toMap ) )

  val genState: Gen[PageState] =
    for {
      inputs   <- genInputs
      inputTab <- Gen.oneOf( InputTab.values )
      outputTab <- Gen.oneOf(
                    Vector( OutputTab.BlocksTab, OutputTab.ResourcesTab, OutputTab.ResourcesTab ) ++ 1
                      .to( 5 )
                      .map( CustomGroup( _ ) )
                  )
      factory      <- Gen.option( Gen.oneOf( Gen.alphaNumStr.map( Left( _ ) ), genFactory.map( Right( _ ) ) ) )
      customGroups <- genCustomGroupSelection
    } yield PageState( inputs, inputTab, outputTab, factory, customGroups )

  "the map options codec" should {
    behave like roundTrip( Codecs.mapOptionsCodec( model ), genMapOptions )
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
    behave like roundTrip( PageState.pageStateCodec( model ), genState )
  }

}