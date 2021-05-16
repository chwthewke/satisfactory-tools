package net.chwthewke.satisfactorytools
package prod

import cats.Id
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.semigroup._
import cats.syntax.traverse._
import mouse.option._

import data.Countable
import data.Item
import model.Bill
import model.ExtractionRecipe
import model.Machine
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
      .solve( inputs.bill, RecipeSelection( model, inputs.recipeList, inputs.options, inputs.resourceOptions ) )
      .map( solutionFactory( inputs.bill, selection, _ ) )
  }

  def renderExtractionRecipes(
      input: Countable[Double, Item],
      tieredRecipes: Vector[ExtractionRecipe]
  ): Option[Vector[ClockedRecipe]] =
    ( Vector.empty[Option[ClockedRecipe]], ( tieredRecipes, input.amount ) )
      .tailRecM[Id, Option[Vector[ClockedRecipe]]] {

        case ( acc, ( tiers, target ) ) =>
          tiers.headOption
            .filter( _ => target.abs > Tolerance )
            .map {
              case ExtractionRecipe( recipe, maxClockSpeed, countBound ) =>
                val ( produced, producing ) =
                  recipe.productsPerMinute
                    .find( _.item == input.item )
                    .cata[( Double, Option[ClockedRecipe] )](
                      c =>
                        if (target > c.amount * countBound * maxClockSpeed / 100d)
                          (
                            c.amount * countBound,
                            ClockedRecipe.overclocked( Countable( recipe, countBound ), maxClockSpeed ).some
                          )
                        else {
                          val extractorCount   = (100d * target / (c.amount * maxClockSpeed)).ceil.toInt
                          val actualClockSpeed = 100d * target / (c.amount * extractorCount)
                          (
                            target,
                            ClockedRecipe.overclocked( Countable( recipe, extractorCount ), actualClockSpeed ).some
                          )
                        },
                      ( 0d, none )
                    )

                ( acc :+ producing, ( tiers.tail, target - produced ) )
            }
            .toLeft( acc.sequence )
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

    def reachable( block: Countable[Double, Recipe[Machine, Item]], fromItems: Set[Item] ): Boolean =
      block.item.ingredients.forall { case Countable( item, _ ) => fromItems( item ) }

    val initialItems: Set[Item] =
      (inputRecipes.foldMap( _.recipe.item.products.toList ).map( _.item ) ++ extraInputs.map( _.item )).to( Set )

    val sortedBlocks: Vector[Countable[Double, Recipe[Machine, Item]]] =
      (
        Vector.empty[Countable[Double, Recipe[Machine, Item]]],
        initialItems,
        solution.recipes
          .filter( _.amount > Tolerance )
          .partition( reachable( _, initialItems ) )
      ).tailRecM[Id, Vector[Countable[Double, Recipe[Machine, Item]]]] {
        case ( acc, seen, ( available, rest ) ) =>
          if (available.isEmpty)
            Right( acc ++ rest )
          else {
            val next     = acc ++ available
            val nextSeen = seen.union( available.foldMap( _.item.products.map( _.item ).toList.to( Set ) ) )

            Left( ( next, nextSeen, rest.partition( reachable( _, nextSeen ) ) ) )
          }
      }

    Factory( inputRecipes.filter( _.recipe.amount > Tolerance ), sortedBlocks, extraInputs, extraOutputs )
  }

}
