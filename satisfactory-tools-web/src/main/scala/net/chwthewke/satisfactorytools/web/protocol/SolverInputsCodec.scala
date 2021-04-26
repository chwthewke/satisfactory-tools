package net.chwthewke.satisfactorytools
package web.protocol

import cats.data.Chain
import cats.data.Validated.Valid
import cats.data.ValidatedNel
import cats.syntax.alternative._
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.functorFilter._
import cats.syntax.option._
import cats.syntax.traverse._
import enumeratum.Enum
import enumeratum.EnumEntry
import org.http4s.FormDataDecoder
import org.http4s.ParseFailure
import scala.collection.Factory
import scodec.Codec
import scodec.bits.Bases.Alphabets
import scodec.bits.BitVector
import scodec.codecs

import model.Bill
import model.Countable
import model.ExtractorType
import model.Item
import model.MapOptions
import model.Model
import model.Options
import model.Options.Belt
import model.Options.ClockSpeed
import model.Options.Extractors
import model.Options.Miner
import model.Options.Pipe
import model.RecipeList
import model.ResourceDistrib
import model.ResourcePurity
import model.SolverInputs

object SolverInputsCodec {

  def toBase64( model: Model, inputs: SolverInputs ): String =
    dataCodec( model )
      .encode( encode( model, inputs ) )
      .map( _.toBase64( Alphabets.Base64Url ) )
      .toOption
      .orEmpty

  def fromBase64( model: Model, string: String ): Either[String, SolverInputs] =
    BitVector
      .fromBase64Descriptive( string, Alphabets.Base64Url )
      .flatMap( dataCodec( model ).decode( _ ).toEither.leftMap( _.message ).map( _.value ) )
      .map( decode( model, _ ) )

  val billDataCodec: Codec[Vector[( Byte, Float )]] = codecs.vectorOfN( codecs.uint8, codecs.byte ~~ codecs.float )

  def recipeListCodec( model: Model ): Codec[RecipeList] =
    subsetCodec[Vector]( model.manufacturingRecipes )
      .xmap( RecipeList( _ ), _.recipes )

  val optionsCodec: Codec[Options] =
    (enumCodec[Belt] ~~
      enumCodec[Pipe] ~~
      enumCodec[Miner] ~~
      enumCodec[ClockSpeed] ~~
      enumSetCodec[Extractors] ~~
      enumSetCodec[Extractors])
      .xmap(
        (Options.apply _).tupled,
        options =>
          ( options.belt, options.pipe, options.miner, options.clockSpeed, options.extractors, options.preferFracking )
      )

  val mapDataCodec: Codec[Vector[( Byte, Byte, Short, Short, Short )]] =
    codecs.vectorOfN( codecs.uint8, codecs.byte ~~ codecs.byte ~~ codecs.short16 ~~ codecs.short16 ~~ codecs.short16 )

  def dataCodec( model: Model ): Codec[Data] =
    (billDataCodec ~~
      recipeListCodec( model ) ~~
      optionsCodec ~~
      mapDataCodec)
      .xmap( (Data.apply _).tupled, data => ( data.bill, data.recipeList, data.optionsData, data.mapData ) )

  // TODO clean up remove the extra encoding spec
  case class Data(
      bill: Vector[( Byte, Float )],
      recipeList: RecipeList,
      optionsData: Options,
      mapData: Vector[( Byte, Byte, Short, Short, Short )]
  )

  def encode( model: Model, inputs: SolverInputs ): Data = {
    val modelItems = model.items.values.toVector

    Data(
      encodeBill( modelItems, inputs.bill ),
      inputs.recipeList,
      inputs.options,
      encodeMapData( modelItems, inputs.mapOptions )
    )
  }

  def decode( model: Model, data: Data ): SolverInputs = {
    val modelItems = model.items.values.toVector

    SolverInputs(
      decodeBill( modelItems, data.bill ),
      data.recipeList,
      data.optionsData,
      decodeMapData( modelItems, data.mapData )
    )
  }

  def encodeMapData(
      modelItems: Vector[Item],
      mapOptions: MapOptions
  ): Vector[( Byte, Byte, Short, Short, Short )] =
    mapOptions.resourceNodes.toVector
      .foldMap {
        case ( extractor, nodes ) =>
          nodes.map {
            case ( item, ResourceDistrib( impureNodes, normalNodes, pureNodes ) ) =>
              (
                ExtractorType.indexOf( extractor ).toByte,
                modelItems.indexOf( item ).toByte,
                impureNodes.toShort,
                normalNodes.toShort,
                pureNodes.toShort
              )
          }.toVector
      }

  def decodeMapData(
      modelItems: Vector[Item],
      mapData: Vector[( Byte, Byte, Short, Short, Short )]
  ): MapOptions =
    MapOptions(
      mapData
        .map {
          case ( extractorIx, itemIx, impure, normal, pure ) =>
            (
              ExtractorType.values( extractorIx & 0xFF ),
              ( modelItems( itemIx & 0xFF ), ResourceDistrib( impure & 0xFFFF, normal & 0xFFFF, pure & 0xFFFF ) )
            )
        }
        .groupMap( _._1 )( _._2 )
        .map { case ( k, v ) => ( k, v.toMap ) }
    )

  def enumCodec[E <: EnumEntry]( implicit E: Enum[E] ): Codec[E] = {
    val size = (32 - java.lang.Integer.numberOfLeadingZeros( E.values.size - 1 )).toLong

    def enumToBits( e: E ): BitVector    = BitVector( E.valuesToIndex( e ) ).takeRight( size )
    def enumOfBits( bits: BitVector ): E = E.values( bits.toInt( signed = false ) )

    codecs.bits( size ).xmap( enumOfBits, enumToBits )
  }

  def enumSetCodec[E <: EnumEntry]( implicit E: Enum[E] ): Codec[Set[E]] =
    subsetCodec[Set]( E.values )

  def subsetCodec[CC[x] <: Iterable[x]] = new SubsetCodecPartiallyApplied[CC]

  class SubsetCodecPartiallyApplied[CC[x] <: Iterable[x]] {

    def apply[A]( values: IndexedSeq[A] )( implicit F: Factory[A, CC[A]] ): Codec[CC[A]] = {
      def setToBitVector( subset: CC[A] ): BitVector = BitVector.bits( values.map( subset.toSet ) )
      def setOfBitVector( bits: BitVector ): CC[A] =
        values.zip( bits.toIndexedSeq ).filter( _._2 ).map( _._1 ).to( F )

      codecs.bits( values.size.toLong ).xmap( setOfBitVector, setToBitVector )
    }
  }

  def encodeBill( modelItems: Vector[Item], bill: Bill ): Vector[( Byte, Float )] =
    bill.items.map {
      case Countable( item, amount ) =>
        ( modelItems.indexOf( item ).toByte, amount.toFloat )
    }

  def decodeBill( modelItems: Vector[Item], data: Vector[( Byte, Float )] ): Bill =
    Bill( data.map { case ( ix, am ) => Countable( modelItems( ix & 0xFF ), am.toDouble ) } )

  private def validateEnum[E <: EnumEntry]( v: String )( implicit E: Enum[E] ): ValidatedNel[ParseFailure, E] =
    E.withNameEither( v ).leftMap( ex => ParseFailure( "", ex.getMessage ) ).toValidatedNel

  def enumFormDataDecoder[E <: EnumEntry]( key: String )( implicit E: Enum[E] ): FormDataDecoder[E] =
    FormDataDecoder
      .field[String]( key )
      .mapValidated( validateEnum[E] )

  def enumSetFormDataDecoder[E <: EnumEntry]( key: String )( implicit E: Enum[E] ): FormDataDecoder[Set[E]] =
    FormDataDecoder( fd => Valid( fd.getOrElse( key, Chain.empty ) ) )
      .mapValidated( strs => strs.traverse( validateEnum[E] ).map( _.toVector.toSet ) )

  def formDataDecoder( model: Model ): FormDataDecoder[SolverInputs] = {
    val bill: FormDataDecoder[Bill] =
      model.items.values.toVector
        .traverse { item =>
          FormDataDecoder
            .fieldOptional[Double]( FormNames.billItem( item ) )
            .map( am => am.filter( _ != 0d ).map( Countable( item, _ ) ) )
        }
        .map( items => Bill( items.unite ) )

    val recipeList =
      FormDataDecoder(
        fd =>
          Valid(
            fd.get( FormNames.recipes )
              .fold( model.manufacturingRecipes )(
                _.mapFilter( s => model.manufacturingRecipes.find( _.className.name == s ) ).toVector
              )
          )
      ).map( RecipeList( _ ) )

    val options = (
      enumFormDataDecoder[Options.Belt]( FormNames.optionsBeltKey ),
      enumFormDataDecoder[Options.Pipe]( FormNames.optionsPipeKey ),
      enumFormDataDecoder[Options.Miner]( FormNames.optionsMinerKey ),
      enumFormDataDecoder[Options.ClockSpeed]( FormNames.optionsClockKey ),
      enumSetFormDataDecoder[Options.Extractors]( FormNames.optionsExtractorsKey ),
      enumSetFormDataDecoder[Options.Extractors]( FormNames.optionsFrackingKey )
    ).mapN(
      Options( _, _, _, _, _, _ )
    )

    val mapOptions =
      ( ExtractorType.values, model.extractedItems, ResourcePurity.values )
        .traverseN(
          ( exT, item, purity ) =>
            FormDataDecoder
              .fieldOptional[Int]( FormNames.extractorItemPurityKey( exT, item, purity ) )
              .map( n => n.map( ( exT, item, purity, _ ) ) )
        )
        .map(
          m =>
            MapOptions(
              m.foldMap(
                  _.foldMap { case ( exT, item, purity, n ) => Map( exT -> Map( item -> Map( purity -> n ) ) ) }
                )
                .map {
                  case ( exT, items ) =>
                    ( exT, items.map {
                      case ( item, purities ) =>
                        (
                          item,
                          ResourceDistrib(
                            purities.getOrElse( ResourcePurity.Impure, 0 ),
                            purities.getOrElse( ResourcePurity.Normal, 0 ),
                            purities.getOrElse( ResourcePurity.Pure, 0 )
                          )
                        )
                    } )
                }
            )
        )

    ( bill, recipeList, options, mapOptions ).mapN( SolverInputs )
  }

}
