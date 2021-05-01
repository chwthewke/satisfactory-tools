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
import cats.syntax.traverse._
import enumeratum.Enum
import enumeratum.EnumEntry
import org.http4s.FormDataDecoder
import org.http4s.ParseFailure
import scodec.Codec
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

object Codecs {

  def inputsCodec( model: Model ): Codec[SolverInputs] =
    (billCodec( model ) ~~ recipeListCodec( model ) ~~ optionsCodec ~~ mapOptionsCodec( model ))
      .xmap(
        (SolverInputs.apply _).tupled,
        inputs => ( inputs.bill, inputs.recipeList, inputs.options, inputs.mapOptions )
      )

  def countableCodec[A]( codec: Codec[A] ): Codec[Countable[A, Double]] =
    (codec ~~ codecs.float.xmap[Double]( _.toDouble, _.toFloat ))
      .xmap( (Countable.apply[A, Double] _).tupled, ct => ( ct.item, ct.amount ) )

  def billCodec( model: Model ): Codec[Bill] =
    codecs
      .vectorOfN( codecs.uint8, countableCodec( itemIndexCodec( model ) ) )
      .xmap( Bill( _ ), _.items )

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

  val distribCodec: Codec[ResourceDistrib] =
    (codecs.uint16 ~~ codecs.uint16 ~~ codecs.uint16)
      .xmap(
        (ResourceDistrib.apply _).tupled,
        rd => ( rd.impureNodes, rd.normalNodes, rd.pureNodes )
      )

  def mapDataPointCodec( model: Model ): Codec[( ExtractorType, ( Item, ResourceDistrib ) )] =
    enumCodec[ExtractorType] ~~ (itemIndexCodec( model ) ~~ distribCodec)

  def mapOptionsCodec( model: Model ): Codec[MapOptions] =
    codecs
      .vectorOfN( codecs.uint8, mapDataPointCodec( model ) )
      .xmap(
        v => MapOptions( v.groupMap( _._1 )( _._2 ).map { case ( exT, items ) => ( exT, items.toMap ) } ),
        _.resourceNodes.toVector.flatMap { case ( exT, items ) => items.toVector.tupleLeft( exT ) }
      )

  val mapDataCodec: Codec[Vector[( Byte, Byte, Short, Short, Short )]] =
    codecs.vectorOfN( codecs.uint8, codecs.byte ~~ codecs.byte ~~ codecs.short16 ~~ codecs.short16 ~~ codecs.short16 )

  def itemIndexCodec( model: Model ): Codec[Item] = {
    val modelItems = model.items.values.toVector

    codecs.uint8.xmap( modelItems( _ ), modelItems.indexOf )
  }

  private def validateEnum[E <: EnumEntry]( v: String )( implicit E: Enum[E] ): ValidatedNel[ParseFailure, E] =
    E.withNameEither( v ).leftMap( ex => ParseFailure( "", ex.getMessage ) ).toValidatedNel

  def enumFormDataDecoder[E <: EnumEntry]( key: String )( implicit E: Enum[E] ): FormDataDecoder[E] =
    FormDataDecoder
      .field[String]( key )
      .mapValidated( validateEnum[E] )

  def enumSetFormDataDecoder[E <: EnumEntry]( key: String )( implicit E: Enum[E] ): FormDataDecoder[Set[E]] =
    FormDataDecoder( fd => Valid( fd.getOrElse( key, Chain.empty ) ) )
      .mapValidated( strs => strs.traverse( validateEnum[E] ).map( _.toVector.toSet ) )

  def bill( model: Model ): FormDataDecoder[Bill] =
    model.items.values.toVector
      .traverse { item =>
        FormDataDecoder
          .fieldOptional[Double]( FormNames.billItem( item ) )
          .map( am => am.filter( _ != 0d ).map( Countable( item, _ ) ) )
      }
      .map( items => Bill( items.unite ) )

  def recipeList( model: Model ): FormDataDecoder[RecipeList] =
    FormDataDecoder(
      fd =>
        Valid(
          fd.get( FormNames.recipes )
            .fold( model.manufacturingRecipes )(
              _.mapFilter( s => model.manufacturingRecipes.find( _.className.name == s ) ).toVector
            )
        )
    ).map( RecipeList( _ ) )

  val options: FormDataDecoder[Options] =
    (
      enumFormDataDecoder[Options.Belt]( FormNames.optionsBeltKey ),
      enumFormDataDecoder[Options.Pipe]( FormNames.optionsPipeKey ),
      enumFormDataDecoder[Options.Miner]( FormNames.optionsMinerKey ),
      enumFormDataDecoder[Options.ClockSpeed]( FormNames.optionsClockKey ),
      enumSetFormDataDecoder[Options.Extractors]( FormNames.optionsExtractorsKey ),
      enumSetFormDataDecoder[Options.Extractors]( FormNames.optionsFrackingKey )
    ).mapN(
      Options( _, _, _, _, _, _ )
    )

  def mapOptions( model: Model ): FormDataDecoder[MapOptions] =
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

  def inputsState( model: Model ): FormDataDecoder[SolverInputs] =
    FormDataDecoder
      .field[String]( "state" )
      .mapValidated(
        str => fromBase64( inputsCodec( model ) )( str ).leftMap( msg => ParseFailure( "", msg ) ).toValidatedNel
      )

  def formDataDecoder( model: Model ): FormDataDecoder[SolverInputs] =
    ( bill( model ), recipeList( model ), options, mapOptions( model ) ).mapN( SolverInputs( _, _, _, _ ) )

}
