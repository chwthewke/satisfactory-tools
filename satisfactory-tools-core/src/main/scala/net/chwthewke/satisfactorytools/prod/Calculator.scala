package net.chwthewke.satisfactorytools
package prod

import cats.Id
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.semigroup._
import cats.syntax.show._
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

  def apply[F[_]]( model: Model, inputs: SolverInputs, solver: Solver ): String =
    computeFactory( model, inputs, solver ).map( _.show ).merge

  def computeFactory(
      model: Model,
      inputs: SolverInputs,
      solver: Solver
  ): Either[String, Factory] = {
    val selection = RecipeSelection( model, inputs.recipeList, inputs.options, inputs.mapOptions )

    solver
      .solve(
        inputs.bill,
        RecipeSelection( model, inputs.recipeList, inputs.options, inputs.mapOptions )
      )
      .map(
        renderFactory( inputs.bill, selection, _ )
      )
  }

  def renderExtractionRecipes(
      input: Countable[Item, Double],
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
                        if (target > c.amount * countBound)
                          (
                            c.amount * countBound,
                            FactoryBlock( Countable( recipe, countBound.toDouble ), clockSpeed ).some
                          )
                        else
                          ( target, FactoryBlock( Countable( recipe, target / c.amount ), clockSpeed ).some ),
                      ( 0d, none )
                    )

                ( acc :+ producing, ( tiers.tail, target - produced ) )
            }
            .toLeft( acc.sequence )
      }

  def renderFactory(
      bill: Bill,
      recipeSelection: RecipeSelection,
      solution: Solution
  ): Factory = {

    val extraOutputs =
      (solution.recipes
        .filter( _.amount > Tolerance )
        .foldMap( r => r.item.reducedItemsPerMinute.map( _.map( _ * r.amount ) ) ) |+|
        bill.items.map { case Countable( item, amount ) => ( item, -amount ) }.toMap)
        .filter( _._2 > Tolerance )
        .map { case ( item, amount ) => Countable( item, amount ) }
        .toVector

    val ( inputRecipes, extraInputs ): (
        Vector[FactoryBlock],
        Vector[Countable[Item, Double]]
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

    val sortedBlocks: Vector[FactoryBlock] =
      (
        Vector.empty[FactoryBlock],
        ( inputRecipes, solution.recipes.filter( _.amount.abs > Tolerance ).map( FactoryBlock( _, 100d ) ) )
      ).tailRecM[Id, Vector[FactoryBlock]] {
        case ( acc, ( available, rest ) ) =>
          if (available.isEmpty)
            Right( acc ++ rest )
          else {
            val next = acc ++ available
            Left(
              (
                next,
                rest.partition(
                  block =>
                    block.recipe.item.ingredients
                      .forall( it => next.exists( _.recipe.item.product.exists( _.item == it.item ) ) )
                )
              )
            )
          }
      }

    Factory( bill, sortedBlocks, extraInputs, extraOutputs )
  }

}
