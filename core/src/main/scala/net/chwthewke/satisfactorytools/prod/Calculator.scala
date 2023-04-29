package net.chwthewke.satisfactorytools
package prod

import cats.Id
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.semigroup._
import mouse.option._
import scala.annotation.tailrec

import data.Countable
import data.Item
import model.Bill
import model.ExtractionRecipe
import model.Model
import model.Recipe
import text.FactoryTable

object Calculator {
  val Tolerance: Double = 1e-6

  def apply( model: Model, inputs: SolverInputs, solver: Solver ): String =
    computeFactory( model, inputs, solver ).map( FactoryTable.render( inputs.bill, _ ) ).merge

  def computeFactory(
      model: Model,
      inputs: SolverInputs,
      solver: Solver
  ): Either[String, Factory] = {
    val selection = RecipeSelection( model, inputs.recipeList, inputs.options, inputs.resourceOptions )

    solver
      .solve( inputs.bill, selection )
      .map( solutionFactory( inputs.bill, selection, _ ) )
  }

  def renderExtractionRecipes(
      input: Countable[Double, Item],
      tieredRecipes: List[ExtractionRecipe]
  ): Option[Vector[ClockedRecipe]] = {
    @tailrec
    def selectRecipes(
        acc: Vector[Countable[Int, ExtractionRecipe]],
        recipes: List[ExtractionRecipe],
        produced: Double
    ): Option[( Vector[Countable[Int, ExtractionRecipe]], Double )] =
      if (produced >= input.amount) ( acc, produced ).some
      else
        recipes match {
          case Nil => none
          case head :: tail =>
            val required: Int =
              ((input.amount - produced) / head.maxAmountPerExtractor).ceil.toInt.min( head.limit )

            selectRecipes( acc :+ Countable( head, required ), tail, produced + head.maxAmountPerExtractor * required )
        }

    Option.when( input.amount > Tolerance )( () ) *>
      selectRecipes( Vector.empty, tieredRecipes, 0d )
        .map {
          case ( recipes, maxAmount ) =>
            val clockSpeed: Double = 100d * input.amount / maxAmount
            recipes.map( extraction => ClockedRecipe.overclocked( extraction.map( _.recipe ), clockSpeed ) )
        }
  }

  def solutionFactory(
      bill: Bill,
      recipeSelection: RecipeSelection,
      solution: Solution
  ): Factory = {

    val extraOutputs =
      (solution.recipes
        .filter( _.amount > Tolerance )
        .foldMap( r => r.item.itemsPerMinuteMap.map( _.map( _ * r.amount ) ) ) |+|
        bill.items.map { case Countable( item, amount ) => ( item, -amount ) }.toMap)
        .filter( _._2 > Tolerance )
        .map { case ( item, amount ) => Countable( item, amount ) }
        .toVector

    val ( inputRecipes, extraInputs ): (
        Vector[ClockedRecipe],
        Vector[Countable[Double, Item]]
    ) =
      solution.inputs
        .filter( _.amount.abs > Tolerance )
        .foldMap(
          input =>
            recipeSelection.tieredExtractionRecipes
              .get( input.item )
              .flatMap( renderExtractionRecipes( input, _ ) )
              .cata(
                r => ( r, Vector.empty ),
                ( Vector.empty, Vector( input ) )
              )
        )

    def reachable( block: Countable[Double, Recipe], fromItems: Set[Item] ): Boolean =
      block.item.ingredients.forall { case Countable( item, _ ) => fromItems( item ) }

    val initialItems: Set[Item] =
      (inputRecipes.foldMap( _.recipe.item.products.toList ).map( _.item ) ++ extraInputs.map( _.item )).to( Set )

    val sortedBlocks: Vector[Countable[Double, Recipe]] =
      (
        Vector.empty[Countable[Double, Recipe]],
        initialItems,
        solution.recipes
          .filter( _.amount > Tolerance )
          .partition( reachable( _, initialItems ) )
      ).tailRecM[Id, Vector[Countable[Double, Recipe]]] {
        case ( acc, seen, ( available, rest ) ) =>
          if (available.isEmpty)
            Right( acc ++ rest )
          else {
            val next     = acc ++ available
            val nextSeen = seen.union( available.foldMap( _.item.products.map( _.item ).toList.to( Set ) ) )

            Left( ( next, nextSeen, rest.partition( reachable( _, nextSeen ) ) ) )
          }
      }

    Factory( inputRecipes.filter( _.fractionalAmount > Tolerance ), sortedBlocks, extraInputs, extraOutputs )
  }

}
