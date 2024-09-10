package net.chwthewke.satisfactorytools
package loader

import cats.Monoid
import cats.Order
import cats.Show
import cats.data.NonEmptyVector
import cats.syntax.alternative._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.functorFilter._
import cats.syntax.option._
import cats.syntax.reducible._
import cats.syntax.show._
import cats.syntax.traverse._
import cats.syntax.vector._
import mouse.option._
import scala.annotation.tailrec

import data.ClassName
import data.GameData
import data.GameRecipe
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
      val schematicDependencies: Map[ClassName /*Schematic*/, Vector[Schematic]],
      val alternateUnlocks: Map[ClassName /*Schematic*/, GameRecipe],
      val baseRecipes: Map[ClassName /*Item*/, GameRecipe],
      val manufacturerSchematics: Map[ClassName /*Manufacturer*/, Schematic]
  ) {

    private def findBaseRecipe( altRecipeSchematic: Schematic ): Option[AnalysisItem] =
      for {
        altRecipe <- alternateUnlocks.get( altRecipeSchematic.className )
        schematic <- findItemBaseRecipe( altRecipe.products.head.item )
      } yield schematic

    def findItemBaseRecipe( itemClass: ClassName ): Option[AnalysisItem] =
      baseRecipes.get( itemClass ).map( AnalysisItem.OfRecipe )

    def analyzeSchematic(
        schematic: Schematic
    ): Either[Set[Milestone], Vector[AnalysisItem]] =
      schematic.`type` match {
        case SchematicType.HardDrive | SchematicType.Shop => Left( Set.empty )
        case SchematicType.Custom | SchematicType.Milestone | SchematicType.Tutorial =>
          Left( Set( Milestone( schematic ) ) )
        case SchematicType.Alternate =>
          Right(
            schematicDependencies.get( schematic.className ).orEmpty.map( AnalysisItem.OfSchematic )
              ++ findBaseRecipe( schematic )
          )
        case SchematicType.Mam =>
          Right(
            schematic.cost.mapFilter( c => findItemBaseRecipe( c.item ) )
          )
      }

    def analyzeRecipe(
        recipe: GameRecipe
    ): Either[Set[Milestone], Vector[AnalysisItem]] =
      Right(
        Vector(
          recipe.producedIn.collectFirstSome( manufacturerSchematics.get ),
          recipeSchematics.get( recipe.className )
        ).unite
          .map( AnalysisItem.OfSchematic )
      )

    val boolOrMonoid: Monoid[Boolean] = new Monoid[Boolean] {
      override def empty: Boolean = false

      override def combine( x: Boolean, y: Boolean ): Boolean = x || y
    }

    @tailrec
    private def loop(
        currentAnalyses: Map[ClassName, Either[Set[Milestone], Vector[AnalysisItem]]]
    ): Map[ClassName, Set[Milestone]] = {

      implicit val m: Monoid[Boolean] = boolOrMonoid

      val ( progressed, newAnalyses ) =
        currentAnalyses.toVector.foldMap {
          case ( cn, toAnalyze ) =>
            toAnalyze
              .flatTraverse( reqs =>
                reqs
                  .foldMapM( item => currentAnalyses.get( item.className ).flatMap( _.left.toOption ) )
                  .cata(
                    milestones => ( true, Left( milestones ) ),
                    ( false, Right( reqs ) )
                  )
              )
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
                  Map( ( cn, e.left.toOption.orEmpty ) )
                )
            }
            .map( _.combineAll )

        // good enough
        notAnalyzed.foreach( na => println( show"INFO [NOT ANALYZED] $na" ) )

        result
      }

    }

    def initAnalyses: Vector[( ClassName, Either[Set[Milestone], Vector[AnalysisItem]] )] =
      data.schematics.map( schematic => ( schematic.className, analyzeSchematic( schematic ) ) ) ++
        data.recipes.map( recipe => ( recipe.className, analyzeRecipe( recipe ) ) )

    def run: Map[ClassName, RecipeCategory] = {
      val analysis: Map[ClassName, Set[Milestone]] = loop( initAnalyses.toMap )

      manufacturingRecipes.mapFilter { recipe =>
        val tier: Int =
          analysis
            .get( recipe.className )
            .flatMap( _.toVector.maximumOption )
            .fold( 0 )( _.tier )

        recipeSchematics
          .get( recipe.className )
          .flatMap[RecipeCategory]( s =>
            s.`type` match {
              case SchematicType.Milestone | SchematicType.Tutorial | SchematicType.Custom =>
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
          case SchematicType.Mam       => researchCategoryOf( schematic ).fold( 99 )( _ => 0 )
          case SchematicType.Milestone => 1
          case SchematicType.Tutorial  => 1
          case SchematicType.Custom    => 2
          case SchematicType.Alternate => 3
          case SchematicType.HardDrive => 99
          case SchematicType.Shop      => 99
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

      val schematicDependencies: Map[ClassName /*Schematic*/, Vector[Schematic]] = {
        val schematicsByClassName: Map[ClassName, Schematic] =
          data.schematics.map( s => ( s.className, s ) ).toMap

        def dependenciesOf( schematic: Schematic ): Vector[Schematic] =
          schematic.schematicDependencies
            .mapFilter( schematicsByClassName.get )

        data.schematics
          .filter( _.`type` == SchematicType.Alternate )
          .map( schematic => ( schematic.className, dependenciesOf( schematic ) ) )
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

      val baseRecipes: Map[ClassName /*Item*/, GameRecipe] =
        manufacturingRecipes
          .filter( recipe =>
            !recipe.displayName.toLowerCase.startsWith( "alternate" ) &&
              recipeSchematics.get( recipe.className ).forall( _.`type` != SchematicType.Alternate )
          )
          .map( recipe => ( recipe.products.head.item, recipe ) )
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
