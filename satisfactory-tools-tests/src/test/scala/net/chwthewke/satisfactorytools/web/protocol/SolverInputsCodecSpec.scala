package net.chwthewke.satisfactorytools
package web.protocol

import alleycats.std.map._
import cats.effect.unsafe.implicits._
import cats.syntax.apply._
import cats.syntax.traverse._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.cats.implicits._
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.Inside
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import scala.collection.Factory
import scodec.Attempt
import scodec.Codec
import scodec.DecodeResult
import scodec.bits.BitVector

import data.Loader
import model.MapOptions
import model.Options
import model.RecipeList
import model.ResourceDistrib

class SolverInputsCodecSpec
    extends AnyWordSpec
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with Inside
    with TypeCheckedTripleEquals {

  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration( minSuccessful = 100 )

  val model = Loader.io.loadModel.unsafeRunSync()

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
    def apply[A]( src: Seq[A] )( implicit F: Factory[A, CC[A]] ): Gen[CC[A]] =
      Gen.choose( 0, src.size ).flatMap( Gen.pick( _, src ) ).map( _.to( F ) )
  }

  val genMapOptions: Gen[MapOptions] =
    model.defaultMapOptions.resourceNodes
      .traverse( _.traverse {
        case ResourceDistrib( impureNodes, normalNodes, pureNodes ) =>
          ( Gen.choose( 0, impureNodes ), Gen.choose( 0, normalNodes ), Gen.choose( 0, pureNodes ) )
            .mapN( ResourceDistrib( _, _, _ ) )
      } )
      .map( MapOptions( _ ) )

  val genMapData: Gen[Vector[( Byte, Byte, Short, Short, Short )]] =
    genMapOptions.map( SolverInputsCodec.encodeMapData( model.extractedItems, _ ) )

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

  val genBillData: Gen[Vector[( Byte, Float )]] =
    Gen
      .choose( 0, 40 )
      .flatMap( Gen.containerOfN[Vector, ( Byte, Float )]( _, ( arbitrary[Byte], arbitrary[Float] ).tupled ) )

  val genData: Gen[SolverInputsCodec.Data] =
    ( genBillData, genRecipeList, genOptions, genMapOptions )
      .mapN(
        ( b, r, o, m ) => SolverInputsCodec.Data( b, r, o, SolverInputsCodec.encodeMapData( model.extractedItems, m ) )
      )

  "the map data codec" should {
    behave like roundTrip( SolverInputsCodec.mapDataCodec, genMapData )
  }

  "the options codec" should {
    behave like roundTrip( SolverInputsCodec.optionsCodec, genOptions )
  }

  "the recipe list codec" should {
    behave like roundTrip( SolverInputsCodec.recipeListCodec( model ), genRecipeList )
  }

  "the bill data codec" should {
    behave like roundTrip( SolverInputsCodec.billDataCodec, genBillData )
  }

  "the data codec" should {
    behave like roundTrip( SolverInputsCodec.dataCodec( model ), genData )
  }

}
