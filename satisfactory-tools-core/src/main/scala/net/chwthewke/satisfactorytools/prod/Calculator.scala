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

import model.Bill
import model.Countable
import model.ExtractionRecipe
import model.Item
import model.Model
import model.SolverInputs

object Calculator {
  val Tolerance: Double = 1e-6

  def apply( model: Model, inputs: SolverInputs, solver: Solver ): String =
    computeFactory( model, inputs, solver ).map( _.render( inputs.bill ) ).merge

  def computeFactory(
      model: Model,
      inputs: SolverInputs,
      solver: Solver
  ): Either[String, Factory] = {
    val selection = RecipeSelection( model, inputs.recipeList, inputs.options, inputs.mapOptions )

    solver
      .solve( inputs.bill, RecipeSelection( model, inputs.recipeList, inputs.options, inputs.mapOptions ) )
      .map( solutionFactory( inputs.bill, selection, _ ) )
  }

  def renderExtractionRecipes(
      input: Countable[Double, Item],
      tieredRecipes: Vector[ExtractionRecipe]
  ): Option[Vector[FactoryBlock]] =
    ( Vector.empty[Option[FactoryBlock]], ( tieredRecipes, input.amount ) )
      .tailRecM[Id, Option[Vector[FactoryBlock]]] {

        case ( acc, ( tiers, target ) ) =>
          tiers.headOption
            .filter( _ => target.abs > Tolerance )
            .map {
              case ExtractionRecipe( recipe, clockSpeed, countBound ) =>
                val ( produced, producing ) =
                  recipe.productsPerMinute
                    .find( _.item == input.item )
                    .cata[( Double, Option[FactoryBlock] )](
                      c =>
                        if (target > c.amount * countBound * clockSpeed / 100d)
                          (
                            c.amount * countBound,
                            FactoryBlock( Countable( recipe, countBound.toDouble ), clockSpeed ).some
                          )
                        else
                          (
                            target,
                            FactoryBlock( Countable( recipe, 100d * target / (c.amount * clockSpeed) ), clockSpeed ).some
                          ),
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
        Vector[FactoryBlock],
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

    def reachable( block: FactoryBlock, fromItems: Set[Item] ): Boolean =
      block.recipe.item.ingredients.forall { case Countable( item, _ ) => fromItems( item ) }

    val initialItems: Set[Item] =
      (inputRecipes.foldMap( _.recipe.item.product.toList ).map( _.item ) ++ extraInputs.map( _.item )).to( Set )

    val sortedBlocks: Vector[FactoryBlock] =
      (
        Vector.empty[FactoryBlock],
        initialItems,
        solution.recipes
          .filter( _.amount > Tolerance )
          .map( FactoryBlock( _, 100d ) )
          .partition( reachable( _, initialItems ) )
      ).tailRecM[Id, Vector[FactoryBlock]] {
        case ( acc, seen, ( available, rest ) ) =>
          if (available.isEmpty)
            Right( acc ++ rest )
          else {
            val next     = acc ++ available
            val nextSeen = seen.union( available.foldMap( _.recipe.item.product.map( _.item ).toList.to( Set ) ) )

            Left( ( next, nextSeen, rest.partition( reachable( _, nextSeen ) ) ) )
          }
      }

    Factory( inputRecipes.filter( _.recipe.amount > Tolerance ), sortedBlocks, extraInputs, extraOutputs )
  }

}
