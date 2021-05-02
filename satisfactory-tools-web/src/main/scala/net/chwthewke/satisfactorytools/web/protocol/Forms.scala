package net.chwthewke.satisfactorytools
package web.protocol

import cats.data.Chain
import cats.data.Validated.Valid
import cats.syntax.alternative._
import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.functorFilter._
import cats.syntax.show._
import cats.syntax.traverse._
import org.http4s.FormDataDecoder

import model.Bill
import model.Countable
import model.ExtractorType
import model.Item
import model.Machine
import model.MapOptions
import model.Model
import model.Options
import model.Recipe
import model.RecipeList
import model.ResourceDistrib
import model.ResourcePurity
import web.state.CustomGroupSelection

object Forms {

  val state: String = "state"

  def billItem( item: Item ): String = show"bill_${item.className}"

  val recipes: String = "recipes"

  val optionsBeltKey: String  = "options_belt"
  val optionsPipeKey: String  = "options_pipe"
  val optionsMinerKey: String = "options_miner"
  val optionsClockKey: String = "options_clock"

  val optionsExtractorsKey: String = "options_extractors"
  val optionsFrackingKey: String   = "options_prefer_fracking"

  def extractorItemPurityKey( extractorType: ExtractorType, item: Item, purity: ResourcePurity ): String =
    s"distrib_${extractorType.entryName}_${item.className}_${purity.entryName}"

  def outputGroup( model: Model, recipe: Recipe[Machine, Item] ): String =
    s"group_${model.manufacturingRecipes.indexOf( recipe )}"

  ////////////////////
  // DECODERS

  def bill( model: Model ): FormDataDecoder[Bill] =
    model.items.values.toVector
      .traverse { item =>
        FormDataDecoder
          .fieldOptional[Double]( Forms.billItem( item ) )
          .map( am => am.filter( _ != 0d ).map( Countable( item, _ ) ) )
      }
      .map( items => Bill( items.unite ) )

  def recipeList( model: Model ): FormDataDecoder[RecipeList] =
    FormDataDecoder(
      fd =>
        Valid(
          fd.get( Forms.recipes )
            .fold( model.manufacturingRecipes )(
              _.mapFilter( s => model.manufacturingRecipes.find( _.className.name == s ) ).toVector
            )
        )
    ).map( RecipeList( _ ) )

  val options: FormDataDecoder[Options] =
    (
      enumFormDataDecoder[Options.Belt]( Forms.optionsBeltKey ),
      enumFormDataDecoder[Options.Pipe]( Forms.optionsPipeKey ),
      enumFormDataDecoder[Options.Miner]( Forms.optionsMinerKey ),
      enumFormDataDecoder[Options.ClockSpeed]( Forms.optionsClockKey ),
      enumSetFormDataDecoder[Options.Extractors]( Forms.optionsExtractorsKey ),
      enumSetFormDataDecoder[Options.Extractors]( Forms.optionsFrackingKey )
    ).mapN(
      Options( _, _, _, _, _, _ )
    )

  def mapOptions( model: Model ): FormDataDecoder[MapOptions] =
    ( ExtractorType.values, model.extractedItems, ResourcePurity.values )
      .traverseN(
        ( exT, item, purity ) =>
          FormDataDecoder
            .fieldOptional[Int]( Forms.extractorItemPurityKey( exT, item, purity ) )
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

  def customGroups( model: Model ): FormDataDecoder[CustomGroupSelection] =
    FormDataDecoder(
      fd =>
        Valid(
          fd.toVector.foldMap {
            case ( k, Chain( v, _* ) ) =>
              (
                Option
                  .when( k.startsWith( "group_" ) )( Numeric[Int].parseString( k.stripPrefix( "group_" ) ) )
                  .flatten
                  .flatMap( model.manufacturingRecipes.lift( _ ) ),
                Numeric[Int].parseString( v )
              ).tupled.toVector
            case _ => Vector.empty
          }.toMap
        )
    ).map( CustomGroupSelection( _ ) )

}
