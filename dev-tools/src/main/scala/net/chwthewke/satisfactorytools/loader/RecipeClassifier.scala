package net.chwthewke.satisfactorytools
package loader

import cats.Monoid
import cats.MonoidK
import cats.Order
import cats.Show
import cats.Traverse
import cats.data.NonEmptyVector
import cats.derived.semiauto
import cats.syntax.all._
import mouse.option._
import scala.annotation.tailrec

import data.ClassName
import data.Form
import data.GameData
import data.GameRecipe
import data.NativeClass
import data.Schematic
import data.SchematicType
import model.RecipeCategory
import model.ResearchCategory

case class RecipeClassifier( data: GameData ) {

  def classifyRecipes: Map[ClassName, RecipeCategory] =
    MilestoneAnalyzer.init.run

  import RecipeClassifier._

  class MilestoneAnalyzer(
      val manufacturingRecipes: Vector[GameRecipe],
      val recipeSchematics: Map[ClassName /*GameRecipe*/, Schematic],
      val schematicDependencies: Map[ClassName /*Schematic*/, AllOf[Schematic]],
      val alternateUnlocks: Map[ClassName /*Schematic*/, GameRecipe],
      val baseRecipes: Map[ClassName /*Item*/, GameRecipe],
      val manufacturerSchematics: Map[ClassName /*Manufacturer*/, Schematic]
  ) {

    private def findBaseRecipe( altRecipeSchematic: Schematic ): AllOf[AnalysisItem] =
      alternateUnlocks
        .get( altRecipeSchematic.className )
        .foldMap( altRecipe => findItemBaseRecipe( altRecipe.products.head.item ) )

    def findItemBaseRecipe( itemClass: ClassName ): AllOf[AnalysisItem] =
      baseRecipes
        .get( itemClass )
        .map( r => AllOf( Vector( OneOf( Vector( AnalysisItem.OfRecipe( r ) ) ) ) ) )
        .orEmpty

    def analyzeSchematic(
        schematic: Schematic
    ): Option[Either[Milestone, AllOf[AnalysisItem]]] =
      schematic.`type` match {
        case SchematicType.HardDrive | SchematicType.Shop => None
        case SchematicType.Custom | SchematicType.Customization | SchematicType.Milestone | SchematicType.Tutorial =>
          Some( Left( Milestone( schematic ) ) )
        case SchematicType.Alternate =>
          Some(
            Right(
              schematicDependencies
                .get( schematic.className )
                .orEmpty
                .map( AnalysisItem.OfSchematic )
                .widen[AnalysisItem]
                <+>
                  findBaseRecipe( schematic )
            )
          )
        case SchematicType.Mam =>
          Some( Right( schematic.cost.foldMap( c => findItemBaseRecipe( c.item ) ) ) )
      }

    def analyzeRecipe(
        recipe: GameRecipe
    ): Either[Milestone, AllOf[AnalysisItem]] =
      Right(
        AllOf(
          Vector(
            recipe.producedIn.collectFirstSome( manufacturerSchematics.get ),
            recipeSchematics.get( recipe.className )
          ).unite
            .map( s => OneOf( Vector( AnalysisItem.OfSchematic( s ) ) ) )
        )
      )

    val boolOrMonoid: Monoid[Boolean] = new Monoid[Boolean] {
      override def empty: Boolean = false

      override def combine( x: Boolean, y: Boolean ): Boolean = x || y
    }

    @tailrec
    private def loop(
        currentAnalyses: Map[ClassName, Either[Milestone, AllOf[AnalysisItem]]]
    ): Map[ClassName, Milestone] = {

      implicit val m: Monoid[Boolean] = boolOrMonoid

      val ( progressed, newAnalyses ) =
        currentAnalyses.toVector.foldMap {
          case ( cn, toAnalyze ) =>
            toAnalyze
              .flatTraverse { reqs =>
                reqs
                  .traverse( item => currentAnalyses.get( item.className ).flatMap( _.left.toOption ) )
                  .map( _.items.mapFilter( _.items.minimumOption ).maximumOption.getOrElse( Milestone( 0 ) ) )
                  .cata(
                    milestone => ( true, Left( milestone ) ),
                    ( false, Right( reqs ) )
                  )
              }
              .map( analyzed => Map( ( cn, analyzed ) ) )
        }

      if (progressed)
        loop( newAnalyses )
      else {
        val ( notAnalyzed, result ) =
          newAnalyses.toVector
            .traverse {
              case ( cn, e ) =>
                (
                  e.toOption.foldMap( _ => Set( cn ) ),
                  Map( ( cn, e.swap.getOrElse( Milestone( 0 ) ) ) )
                )
            }
            .map( _.foldLeft( Map.empty[ClassName, Milestone] )( _ ++ _ ) )

        // good enough
        notAnalyzed.foreach( na => println( show"INFO [NOT ANALYZED] $na" ) )

        result
      }

    }

    def analyze: Map[ClassName, Milestone] = loop( initAnalyses.toMap )

    def initAnalyses: Vector[( ClassName, Either[Milestone, AllOf[AnalysisItem]] )] =
      data.schematics.mapFilter( schematic => analyzeSchematic( schematic ).tupleLeft( schematic.className ) ) ++
        data.recipes.map( recipe => ( recipe.className, analyzeRecipe( recipe ) ) )

    def run: Map[ClassName, RecipeCategory] = {
      val analysis: Map[ClassName, Milestone] = analyze

      manufacturingRecipes.mapFilter { recipe =>
        val tier: Int =
          analysis
            .get( recipe.className )
            .fold( 0 )( _.tier )

        recipeSchematics
          .get( recipe.className )
          .flatMap[RecipeCategory]( s =>
            s.`type` match {
              case SchematicType.Milestone |
                  SchematicType.Tutorial | SchematicType.Custom | SchematicType.Customization =>
                Some( RecipeCategory.Milestone( tier ) )
              case SchematicType.Alternate =>
                Some( RecipeCategory.Alternate( tier ) )
              case SchematicType.Mam =>
                MilestoneAnalyzer.researchCategoryOf( s ).map( RecipeCategory.Mam( tier, _ ) )
              case SchematicType.HardDrive | SchematicType.Shop => None
            }
          )
          .tupleLeft( recipe.className )

      }.toMap
    }

  }

  object MilestoneAnalyzer {
    private def alterClassName( manufacturerClass: ClassName ): ClassName =
      ClassName( "Desc_" + manufacturerClass.name.stripPrefix( "Build_" ) )

    def researchCategoryOf( schematic: Schematic ): Option[ResearchCategory] =
      ResearchCategory.values
        .find( rc => rc.keys.exists( k => schematic.className.name.startsWith( s"Research_${k}_" ) ) )
        .filter( _ => !schematic.displayName.toLowerCase.startsWith( "discontinued" ) )

    private def canonicalUnlocks( data: GameData ): Map[ClassName /*GameRecipe*/, Schematic] = {
      val allRecipeUnlocks: Map[ClassName, NonEmptyVector[Schematic]] =
        data.schematics.foldMap( s => s.unlocks.tupleRight( NonEmptyVector.one( s ) ).toMap )

      def schematicPriority( schematic: Schematic ) =
        schematic.`type` match {
          case SchematicType.Mam           => researchCategoryOf( schematic ).fold( 99 )( _ => 0 )
          case SchematicType.Milestone     => 1
          case SchematicType.Tutorial      => 1
          case SchematicType.Custom        => 2
          case SchematicType.Customization => 3
          case SchematicType.Alternate     => 4
          case SchematicType.HardDrive     => 99
          case SchematicType.Shop          => 99
        }

      allRecipeUnlocks.fmap( _.minimumBy( schematicPriority ) )
    }

    def init: MilestoneAnalyzer = {
      val manufacturingRecipes: Vector[GameRecipe] = data.recipes.filter( recipe =>
        recipe.producedIn.intersect[ClassName]( data.manufacturers.keys.toSeq ).size == 1
      )

      val manufacturingRecipeClasses: Set[ClassName] = manufacturingRecipes.map( _.className ).toSet

      val recipeSchematics: Map[ClassName /*GameRecipe*/, Schematic] =
        canonicalUnlocks( data )
          .filter { case ( c, _ ) => manufacturingRecipeClasses.contains( c ) }

      val schematicDependencies: Map[ClassName /*Schematic*/, AllOf[Schematic]] = {
        val schematicsByClassName: Map[ClassName, Schematic] =
          data.schematics.map( s => ( s.className, s ) ).toMap

        def dependenciesOf( schematic: Schematic ): Vector[Schematic] =
          schematic.schematicDependencies
            .mapFilter( schematicsByClassName.get )

        data.schematics
          .filter( _.`type` == SchematicType.Alternate )
          .map( schematic =>
            (
              schematic.className,
              if (schematic.requireAllDependencies)
                AllOf( dependenciesOf( schematic ).map( s => OneOf( Vector( s ) ) ) )
              else
                AllOf( Vector( OneOf( dependenciesOf( schematic ) ) ) )
            )
          )
          .toMap
      }

      val alternateUnlocks: Map[ClassName /*Schematic*/, GameRecipe] = {
        val recipesByClassName = data.recipes.map( recipe => ( recipe.className, recipe ) ).toMap

        data.schematics
          .filter( _.`type` == SchematicType.Alternate )
          .mapFilter { schematic =>
            schematic.unlocks
              .mapFilter( recipesByClassName.get )
              .toNev
              .map( _.head )
              .tupleLeft( schematic.className )
          }
          .toMap

      }

      val noBaseRecipes: Set[ClassName] =
        data.items.values.collect {
          case ( item, NativeClass.resourceDescClass ) if item.form == Form.Solid => item.className
        }.toSet

      val baseRecipes: Map[ClassName /*Item*/, GameRecipe] =
        manufacturingRecipes
          .filter( recipe =>
            !recipe.displayName.toLowerCase.startsWith( "alternate" ) &&
              recipeSchematics.get( recipe.className ).forall( _.`type` != SchematicType.Alternate )
          )
          .map( recipe => ( recipe.products.head.item, recipe ) )
          .filterNot { case ( item, _ ) => noBaseRecipes.contains( item ) }
          .toMap

      val manufacturerSchematics: Map[ClassName, Schematic] =
        data.manufacturers.values.toVector
          .mapFilter( manu =>
            data.recipes
              .find( _.products.exists( _.item == alterClassName( manu.className ) ) )
              .flatMap( recipe =>
                data.schematics
                  .find( schem => schem.unlocks.contains( recipe.className ) )
              )
              .tupleLeft( manu.className )
          )
          .toMap

      new MilestoneAnalyzer(
        manufacturingRecipes,
        recipeSchematics,
        schematicDependencies,
        alternateUnlocks,
        baseRecipes,
        manufacturerSchematics
      )

    }
  }
}

object RecipeClassifier {

  case class OneOf[+A]( items: Vector[A] )
  object OneOf {
    implicit val oneOfTraverse: Traverse[OneOf]   = semiauto.traverse[OneOf]
    implicit val oneOfMonoidK: MonoidK[OneOf]     = semiauto.monoidK[OneOf]
    implicit def oneOfMonoid[A]: Monoid[OneOf[A]] = oneOfMonoidK.algebra
  }
  case class AllOf[+A]( items: Vector[OneOf[A]] )
  object AllOf {
    implicit val allOfTraverse: Traverse[AllOf]   = semiauto.traverse[AllOf]
    implicit val allOfMonoidK: MonoidK[AllOf]     = semiauto.monoidK[AllOf]
    implicit def allOfMonoid[A]: Monoid[AllOf[A]] = allOfMonoidK.algebra
  }

  case class Milestone( tier: Int ) extends AnyVal
  object Milestone {
    def apply( schematic: Schematic ): Milestone        = Milestone( schematic.techTier )
    implicit val milestoneOrder: Order[Milestone]       = Order.by( _.tier )
    implicit val milestoneOrdering: Ordering[Milestone] = Order.catsKernelOrderingForOrder
  }

  sealed trait AnalysisItem {
    def className: ClassName

    def displayName: String

    override def toString: String = show"$displayName [$className]"
  }

  object AnalysisItem {
    case class OfRecipe( recipe: GameRecipe ) extends AnalysisItem {
      override def className: ClassName = recipe.className

      override def displayName: String = recipe.displayName
    }

    case class OfSchematic( schematic: Schematic ) extends AnalysisItem {
      override def className: ClassName = schematic.className

      override def displayName: String = schematic.displayName

      override def toString: String = super.toString + " " + schematic.`type`
    }

    implicit val analysisItemShow: Show[AnalysisItem] = Show.fromToString
  }
}
