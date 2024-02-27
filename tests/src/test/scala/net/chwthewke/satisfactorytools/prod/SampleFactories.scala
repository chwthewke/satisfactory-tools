package net.chwthewke.satisfactorytools
package prod

import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.traverse._
import org.scalacheck.Gen
import org.scalacheck.cats.implicits._
import scala.annotation.tailrec

import data.Countable
import data.Item
import model.Bill
import model.ExtractorType
import model.Model
import model.Options
import model.Recipe
import model.RecipeList
import model.ResourceOptions
import prod.ojsolver.ConstraintSolver

case class SampleFactories(
    factories: Vector[Factory]
)

object SampleFactories {
  private case class Generators( model: Model ) {
    val allRecipesList: RecipeList       = RecipeList( model.manufacturingRecipes )
    val allNonAltRecipesList: RecipeList = RecipeList( model.manufacturingRecipes.filter( !_.isAlternate ) )

    val basicOptions: Options =
      Options(
        Options.Belt.BeltMk3,
        Options.Pipe.PipeMk1,
        Options.Miner.MinerMk2,
        Options.ClockSpeed.ClockSpeed100,
        Set( ExtractorType.Miner, ExtractorType.WaterPump ),
        Set.empty
      )

    val resourceOptions: ResourceOptions = model.defaultResourceOptions

    val recipeList: Gen[RecipeList] = Gen.oneOf( allRecipesList, allNonAltRecipesList )
    val options: Gen[Options]       = Gen.oneOf( basicOptions, Options.full )

    def recipeSelection: Gen[RecipeSelection] = {
      ( Gen.oneOf( allRecipesList, allNonAltRecipesList ), Gen.oneOf( basicOptions, Options.full ) ).mapN(
        RecipeSelection( model, _, _, resourceOptions )
      )
    }

    private def extractableItems( recipeSelection: RecipeSelection ): Set[Item] =
      recipeSelection.tieredExtractionRecipes.toVector
        .foldMap {
          case ( item, recipes ) =>
            val isExtractable: Boolean =
              recipes.nonEmpty && recipeSelection.resourceCaps.get( item ).exists( _ > 0d )

            Option.when( isExtractable )( item ).toSet
        }

    private def producibleItems( recipeSelection: RecipeSelection ): Set[Item] = {
      @tailrec
      def loop( producible: Set[Item], recipes: Vector[Recipe] ): Set[Item] = {
        val nextRecipeIx: Int =
          recipes.indexWhere( recipe => recipe.ingredients.forall( c => producible( c.item ) ) )

        if (nextRecipeIx < 0)
          producible
        else {
          val nextRecipes: Vector[Recipe] = recipes.patch( nextRecipeIx, Vector.empty, 1 )
          val nextProducible: Set[Item]   = producible ++ recipes( nextRecipeIx ).products.map( _.item ).toList

          loop( nextProducible, nextRecipes )
        }

      }

      val initItems: Set[Item] = extractableItems( recipeSelection )
      loop( initItems, recipeSelection.allowedRecipes.map( _._1 ) )
        .diff( initItems )
    }

    def bill( recipeList: RecipeList, options: Options ): Gen[Bill] =
      for {
        nItems <- Gen.choose( 1, 8 )
        items  <- Gen.pick( nItems, producibleItems( RecipeSelection( model, recipeList, options, resourceOptions ) ) )
        amounts <- items.toVector.traverse(
                    item =>
                      Gen
                        .choose( 1, 10 )
                        .map( _ * (if (item.sinkPoints > 100) 1d else 10d) )
                        .map( amt => Countable( item, amt ) )
                  )
      } yield Bill( amounts )

    def solverInputs: Gen[SolverInputs] =
      for {
        list <- recipeList
        opts <- options
        b    <- bill( list, opts )
      } yield SolverInputs( b, list, opts, resourceOptions )

    def factory: Gen[Factory] =
      solverInputs
        .map( i => Calculator.computeFactory( model, i, ConstraintSolver ) )
        .retryUntil( _.isRight, 10 )
        .flatMap( _.fold( _ => Gen.fail[Factory], Gen.const ) )

  }

  def genFactories( model: Model ): Gen[SampleFactories] =
    Generators( model ).factory.replicateA( 4 ).map( fs => SampleFactories( fs.toVector ) )

}
