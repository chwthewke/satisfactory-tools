package net.chwthewke.satisfactorytools
package web

import cats.data.Chain
import cats.data.Validated.Valid
import cats.data.ValidatedNel
import cats.syntax.alternative._
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.functorFilter._
import cats.syntax.show._
import cats.syntax.traverse._
import enumeratum.Enum
import enumeratum.EnumEntry
import org.http4s.FormDataDecoder
import org.http4s.ParseFailure

import data.ClassName
import data.Countable
import data.Item
import model.Bill
import model.ExtractorType
import model.Model
import model.Options
import model.RecipeList
import model.ResourceDistrib
import model.ResourceOptions
import model.ResourcePurity
import model.ResourceWeights
import protocol.InputTab
import protocol.ModelVersionId
import protocol.OutputTab
import protocol.PlanId
import protocol.PlanName

object forms {

  object Keys {
    abstract class Prefix[A] private[forms] ( prefix: String, to: A => String, from: String => Option[A] ) {
      def apply( value: A ): String = s"${prefix}_${to( value )}"

      def unapply( string: String ): Option[A] =
        Option.when( string.startsWith( s"${prefix}_" ) )( string.stripPrefix( s"${prefix}_" ) ).flatMap( from )
    }

    val planTitle: String = "plan_title"

    def billItem( item: Item ): String = show"bill_${item.className}"

    val recipes: String = "recipes"

    val optionsBeltKey: String  = "options_belt"
    val optionsPipeKey: String  = "options_pipe"
    val optionsMinerKey: String = "options_miner"
    val optionsClockKey: String = "options_clock"

    val optionsExtractorsKey: String = "options_extractors"
    val optionsFrackingKey: String   = "options_prefer_fracking"

    def extractorItemPurityKey( extractorType: ExtractorType, item: Item, purity: ResourcePurity ): String =
      s"distrib_${extractorType.entryName}_${item.className.name}_${purity.entryName}"

    def resourceWeightKey( item: Item ): String =
      s"weight_${item.className.name}"

    object outputGroup extends Prefix[ClassName]( "group", _.show, s => Some( ClassName( s ) ) )

    val outputGroupCount: String = "group_count"

    val compareBefore: String = "cmp_before"
    val compareAfter: String  = "cmp_after"

    val modelVersion: String = "model_version"
  }

  object Actions {

    object input {
      def apply( tab: InputTab ): String = tab match {
        case InputTab.Bill            => "bill"
        case InputTab.Recipes         => "recipes"
        case InputTab.Options         => "options"
        case InputTab.ResourceOptions => "resources"
      }

      def unapply( string: String ): Option[InputTab] =
        InputTab.values.find( apply( _ ) == string )
    }

    object output {
      def apply( tab: OutputTab ): String = tab match {
        case OutputTab.Steps             => "steps"
        case OutputTab.Items             => "items"
        case OutputTab.Machines          => "machines"
        case OutputTab.Inputs            => "inputs"
        case OutputTab.CustomGroup( ix ) => s"group_$ix"
        case OutputTab.GroupIO           => "group-io"
      }

      def unapply( string: String ): Option[OutputTab] =
        Vector( OutputTab.Steps, OutputTab.Items, OutputTab.Machines, OutputTab.Inputs, OutputTab.GroupIO )
          .find( apply( _ ) == string )
          .orElse(
            Option
              .when( string.startsWith( "group_" ) )( string.stripPrefix( "group_" ) )
              .flatMap( Numeric[Int].parseString )
              .map( OutputTab.CustomGroup )
          )
    }

    val save: String    = "save"
    val migrate: String = "migrate"

    val addAllRecipes: String = "add_all_recipes"
    val addAlts: String       = "add_alts"
    val removeAlts: String    = "remove_alts"
    val lockRecipes: String   = "lock_recipes"

    val compute: String     = "compute"
    val addGroup: String    = "group_inc"
    val removeGroup: String = "group_dec"

    val outputGroupOrder: String = "group_order"

  }

  ////////////////////
  // DECODERS
  object Decoders {

    private def validateEnum[E <: EnumEntry]( v: String )( implicit E: Enum[E] ): ValidatedNel[ParseFailure, E] =
      E.withNameEither( v ).leftMap( ex => ParseFailure( "", ex.getMessage ) ).toValidatedNel

    def enumFormDataDecoder[E <: EnumEntry]( key: String )( implicit E: Enum[E] ): FormDataDecoder[E] =
      FormDataDecoder
        .field[String]( key )
        .mapValidated( validateEnum[E] )

    def enumSetFormDataDecoder[E <: EnumEntry]( key: String )( implicit E: Enum[E] ): FormDataDecoder[Set[E]] =
      FormDataDecoder( fd => Valid( fd.getOrElse( key, Chain.empty ) ) )
        .mapValidated( strs => strs.traverse( validateEnum[E] ).map( _.toVector.toSet ) )

    val modelVersion: FormDataDecoder[ModelVersionId] =
      FormDataDecoder.field[Int]( Keys.modelVersion ).map( ModelVersionId( _ ) )

    val title: FormDataDecoder[Option[PlanName]] =
      FormDataDecoder.fieldOptional[String]( Keys.planTitle ).map( _.filterNot( _.isEmpty ).map( PlanName( _ ) ) )

    def bill( model: Model ): FormDataDecoder[Bill] =
      model.items.values.toVector
        .traverse { item =>
          FormDataDecoder
            .fieldOptional[Double]( Keys.billItem( item ) )
            .map( am => am.filter( _ != 0d ).map( Countable( item, _ ) ) )
        }
        .map( items => Bill( items.unite ) )

    def recipeList( model: Model ): FormDataDecoder[RecipeList] =
      FormDataDecoder(
        fd =>
          Valid(
            fd.get( Keys.recipes )
              .fold( model.manufacturingRecipes )(
                _.mapFilter( s => model.manufacturingRecipes.find( _.className.name == s ) ).toVector
              )
          )
      ).map( RecipeList( _ ) )

    val options: FormDataDecoder[Options] =
      (
        enumFormDataDecoder[Options.Belt]( Keys.optionsBeltKey ),
        enumFormDataDecoder[Options.Pipe]( Keys.optionsPipeKey ),
        enumFormDataDecoder[Options.Miner]( Keys.optionsMinerKey ),
        enumFormDataDecoder[Options.ClockSpeed]( Keys.optionsClockKey ),
        enumSetFormDataDecoder[ExtractorType]( Keys.optionsExtractorsKey ),
        enumSetFormDataDecoder[ExtractorType]( Keys.optionsFrackingKey )
      ).mapN(
        Options( _, _, _, _, _, _ )
      )

    def resourceNodes( model: Model ): FormDataDecoder[Map[ExtractorType, Map[ClassName, ResourceDistrib]]] =
      ( ExtractorType.values, model.extractedItems, ResourcePurity.values )
        .traverseN(
          ( exT, item, purity ) =>
            FormDataDecoder
              .fieldOptional[Int]( Keys.extractorItemPurityKey( exT, item, purity ) )
              .map( n => n.map( ( exT, item.className, purity, _ ) ) )
        )
        .map(
          m =>
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

    def resourceWeights( model: Model ): FormDataDecoder[ResourceWeights] =
      model.extractedItems
        .traverse(
          item =>
            FormDataDecoder
              .fieldOptional[Int]( Keys.resourceWeightKey( item ) )
              .map( o => ( item.className, o.getOrElse( ResourceWeights.range ) ) )
        )
        .map( v => ResourceWeights( v.toMap ) )

    def resourceOptions( model: Model ): FormDataDecoder[ResourceOptions] =
      ( resourceNodes( model ), resourceWeights( model ) ).mapN( ResourceOptions( _, _ ) )

    def customGroups: FormDataDecoder[Map[ClassName, Int]] =
      FormDataDecoder(
        fd =>
          Valid(
            fd.toVector.foldMap {
              case ( k, Chain( v, _* ) ) =>
                (
                  Keys.outputGroup.unapply( k ),
                  Numeric[Int].parseString( v )
                ).tupled.toVector
                  .filter( _._2 != 0 )
              case _ => Vector.empty
            }.toMap
          )
      )

    val comparePlans: FormDataDecoder[Option[( PlanId, PlanId )]] =
      (
        FormDataDecoder.fieldOptional[Int]( Keys.compareBefore ),
        FormDataDecoder.fieldOptional[Int]( Keys.compareAfter )
      ).mapN( ( bo, ao ) => ( bo, ao ).mapN( ( b, a ) => ( PlanId( b ), PlanId( a ) ) ) )

  }
}
