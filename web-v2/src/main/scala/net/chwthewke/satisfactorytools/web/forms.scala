package net.chwthewke.satisfactorytools
package web

import cats.data.Chain
import cats.data.Validated.Valid
import cats.data.ValidatedNel
import cats.effect.Async
import cats.parse.Parser
import cats.syntax.alternative._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.functorFilter._
import cats.syntax.option._
import cats.syntax.show._
import cats.syntax.traverse._
import enumeratum.Enum
import enumeratum.EnumEntry
import org.http4s.FormDataDecoder
import org.http4s.ParseFailure
import org.http4s.QueryParamDecoder
import org.http4s.QueryParamEncoder
import org.http4s.Request
import org.http4s.Uri
import org.http4s.syntax.literals._

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
import prod.tree.PushDownType
import prod.tree.TreeCommand
import prod.tree.TreeLoc
import protocol.InputTab
import protocol.ModelVersionId
import protocol.OutputTab
import protocol.PlanId
import protocol.PlanName

object forms {

  // TODO merge with the other enum decoder in forms.Decoders
  import FormDataDecoder.formEntityDecoder

  implicit def enumQueryParamEncoder[E <: EnumEntry: Enum]: QueryParamEncoder[E] =
    QueryParamEncoder[String].contramap[E]( _.entryName )

  implicit def enumQueryParamDecoder[E <: EnumEntry]( implicit E: Enum[E] ): QueryParamDecoder[E] =
    QueryParamDecoder[String]
      .emap( s =>
        E.withNameOption( s )
          .toRight( ParseFailure( s"Invalid enum string", s"Invalid enum string '$s'" ) )
      )

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
        case OutputTab.Tree              => "tree"
      }

      def unapply( string: String ): Option[OutputTab] =
        Vector(
          OutputTab.Steps,
          OutputTab.Items,
          OutputTab.Machines,
          OutputTab.Inputs,
          OutputTab.GroupIO,
          OutputTab.Tree
        ).find( apply( _ ) == string )
          .orElse(
            Option
              .when( string.startsWith( "group_" ) )( string.stripPrefix( "group_" ) )
              .flatMap( Numeric[Int].parseString )
              .map( OutputTab.CustomGroup )
          )
    }

    object tree {
      val destroyPath: Uri.Path     = path"tree_destroy"
      val pullUpPath: Uri.Path      = path"tree_pull_up"
      val pushDownPath: Uri.Path    = path"tree_push_down"
      val pushDownForPath: Uri.Path = path"tree_push_down_for"

      val resetPath: Uri.Path = path"reset"

      import org.http4s.dsl.request._

      object location {
        val root: String = "root"

        def apply( loc: TreeLoc ): String = loc match {
          case TreeLoc.Root => root
          case _            => loc.toString
        }

        def unapply( str: String ): Option[TreeLoc] =
          TreeLoc.parse( str ).orElse( Option.when( str == root )( TreeLoc.Root ) )
      }

      def reset: String =
        resetPath.renderString

      def pullUp( from: TreeLoc.NonRoot, recipe: ClassName ): String =
        ( pullUpPath / location( from ) / recipe.name ).renderString

      def destroy( at: TreeLoc.NonRoot ): String =
        ( destroyPath / location( at ) ).renderString

      def pushDown( from: TreeLoc, ix: Int, recipe: ClassName ): String =
        ( pushDownPath / location( from ) / ix.toString / recipe.name ).renderString

      def pushDownFor( from: TreeLoc, ix: Int, recipe: ClassName ): String =
        ( pushDownForPath / location( from ) / ix.toString / recipe.name ).renderString

      def pushDownTargetDropdown( loc: TreeLoc, ix: Int ): String =
        s"push_down_target_${ix}_$loc"

      def pushDownRadio( loc: TreeLoc, ix: Int ): String =
        s"push_down_type_${ix}_$loc"

      def pushDownFractionDropdown( loc: TreeLoc, ix: Int ): String =
        s"push_down_fraction_${ix}_$loc"

      def pushDownForDropdown( loc: TreeLoc, ix: Int ): String =
        s"push_down_for_${ix}_$loc"

      def pushDownAmountInput( loc: TreeLoc, ix: Int ): String =
        s"push_down_amount_${ix}_$loc"

      sealed abstract class RegularPushDownTypeChoice( override val entryName: String ) extends EnumEntry
      object RegularPushDownTypeChoice extends Enum[RegularPushDownTypeChoice] {
        final case object All      extends RegularPushDownTypeChoice( "all" )
        final case object Fraction extends RegularPushDownTypeChoice( "fraction" )
        final case object Amount   extends RegularPushDownTypeChoice( "amount" )

        override val values: IndexedSeq[RegularPushDownTypeChoice] = findValues
      }

      def regularPushDownTypeDecoder( loc: TreeLoc, ix: Int ): FormDataDecoder[( PushDownType, Int )] =
        (
          FormDataDecoder.field[RegularPushDownTypeChoice]( pushDownRadio( loc, ix ) ),
          FormDataDecoder.field[Int]( pushDownFractionDropdown( loc, ix ) ),
          FormDataDecoder.field[Double]( pushDownAmountInput( loc, ix ) ),
          FormDataDecoder.field[Int]( pushDownTargetDropdown( loc, ix ) )
        ).mapN( ( t, frac, am, tgt ) =>
          (
            t match {
              case RegularPushDownTypeChoice.All      => PushDownType.Full
              case RegularPushDownTypeChoice.Fraction => PushDownType.Fraction( frac )
              case RegularPushDownTypeChoice.Amount   => PushDownType.Amount( am )
            },
            tgt
          )
        )

      case class PushDownForTargetData( className: ClassName, childIndex: Int ) {
        def renderString: String = s"${childIndex}_${className.name}"
      }

      object PushDownForTargetData {
        import cats.parse.Numbers
        val parser: Parser[PushDownForTargetData] =
          (
            Numbers.nonNegativeIntString.mapFilter( _.toIntOption ),
            Parser.char( '_' ),
            Parser.anyChar.rep.string.map( ClassName( _ ) )
          ).mapN( ( ix, _, n ) => PushDownForTargetData( n, ix ) )
      }

      def pushDownForDecoder( loc: TreeLoc, ix: Int ): FormDataDecoder[PushDownForTargetData] =
        FormDataDecoder
          .field[String]( pushDownForDropdown( loc, ix ) )
          .mapValidated( str =>
            PushDownForTargetData.parser
              .parseAll( str )
              .leftMap( err => ParseFailure( s"$str as PushDownForTargetData", err.toString() ) )
              .toValidatedNel
          )

      def command[F[_]: Async]( path: Uri.Path, request: Request[F] ): Option[F[TreeCommand]] =
        path match {
          case `destroyPath` / location( loc ) =>
            loc.nonRoot.map( TreeCommand.Destroy ).map( _.pure[F].widen )
          case `pullUpPath` / location( loc ) / recipe =>
            loc.nonRoot.map( TreeCommand.PullUp( _, ClassName( recipe ) ) ).map( _.pure[F].widen )
          case `pushDownPath` / location( loc ) / IntVar( ix ) / recipe =>
            implicit val decoder: FormDataDecoder[( PushDownType, Int )] = regularPushDownTypeDecoder( loc, ix )
            request
              .as[( PushDownType, Int )]( implicitly, formEntityDecoder )
              .map { case ( tpe, chIx ) => TreeCommand.PushDown( loc.append( chIx ), ClassName( recipe ), tpe ) }
              .widen[TreeCommand]
              .some
          case `pushDownForPath` / location( loc ) / IntVar( ix ) / recipe =>
            implicit val decoder: FormDataDecoder[PushDownForTargetData] = pushDownForDecoder( loc, ix )
            request
              .as[PushDownForTargetData]( implicitly, formEntityDecoder )
              .map {
                case PushDownForTargetData( target, childIndex ) =>
                  TreeCommand.PushDown( loc.append( childIndex ), ClassName( recipe ), PushDownType.For( target ) )
              }
              .widen[TreeCommand]
              .some
          case _ => None
        }

    }

    val save: String    = "save"
    val migrate: String = "migrate"

    val addAllRecipes: String = "add_all_recipes"
    val addAlts: String       = "add_alts"
    val removeAlts: String    = "remove_alts"
    val lockRecipes: String   = "lock_recipes"

    object recipesUpToTier {
      def apply( tier: Int, alternates: Boolean ): String =
        s"add_recipes_tier_$tier" + ( if (alternates) "_alt" else "" )
      def unapply( s: String ): Option[( Int, Boolean )] = {
        import atto.Atto._

        ( string( "add_recipes_tier_" ) ~> int ~ opt( string( "_alt" ) ) )
          .map { case ( tier, alt ) => ( tier, alt.isDefined ) }
          .parseOnly( s )
          .option
      }

    }

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
      FormDataDecoder( fd =>
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
        .traverseN( ( exT, item, purity ) =>
          FormDataDecoder
            .fieldOptional[Int]( Keys.extractorItemPurityKey( exT, item, purity ) )
            .map( n => n.map( ( exT, item.className, purity, _ ) ) )
        )
        .map( m =>
          m.foldMap(
            _.foldMap { case ( exT, item, purity, n ) => Map( exT -> Map( item -> Map( purity -> n ) ) ) }
          ).map {
            case ( exT, items ) =>
              (
                exT,
                items.map {
                  case ( item, purities ) =>
                    (
                      item,
                      ResourceDistrib(
                        purities.getOrElse( ResourcePurity.Impure, 0 ),
                        purities.getOrElse( ResourcePurity.Normal, 0 ),
                        purities.getOrElse( ResourcePurity.Pure, 0 )
                      )
                    )
                }
              )
          }
        )

    def resourceWeights( model: Model ): FormDataDecoder[ResourceWeights] =
      model.extractedItems
        .traverse( item =>
          FormDataDecoder
            .fieldOptional[Int]( Keys.resourceWeightKey( item ) )
            .map( o => ( item.className, o.getOrElse( ResourceWeights.range ) ) )
        )
        .map( v => ResourceWeights( v.toMap ) )

    def resourceOptions( model: Model ): FormDataDecoder[ResourceOptions] =
      ( resourceNodes( model ), resourceWeights( model ) ).mapN( ResourceOptions( _, _ ) )

    def customGroups: FormDataDecoder[Map[ClassName, Int]] =
      FormDataDecoder( fd =>
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
