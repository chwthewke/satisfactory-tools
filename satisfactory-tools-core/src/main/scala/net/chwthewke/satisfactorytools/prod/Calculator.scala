package net.chwthewke.satisfactorytools
package prod

import cats.Id
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.show._
import mouse.option._

import data.ProductionConfig
import model.Countable
import model.Item
import model.Machine
import model.Model
import model.Options
import model.Recipe

object Calculator {
  def apply[F[_]]( model: Model, config: ProductionConfig, options: Options, solver: Solver ): String =
    computeFactory( model, config, options, solver ).map( _.show ).merge

  def computeFactory(
      model: Model,
      config: ProductionConfig,
      options: Options,
      solver: Solver
  ): Either[String, Factory] =
    for {
      bill      <- Bill.init( model, config )
      selection <- RecipeSelection.init( model, config, options )
      resourceCaps = ResourceCaps.init( model, options, selection.extractionRecipes )
      solution <- solver.solve( bill, selection, ResourceWeights.init( resourceCaps ), resourceCaps )
    } yield renderFactory( model, bill, selection, solution, options )

  def renderFactory(
      model: Model,
      bill: Bill,
      recipeSelection: RecipeSelection,
      solution: Solution,
      options: Options
  ): Factory = {

    val ( inputRecipes, extraInputs ): (
        Vector[Countable[Recipe[Machine, Item], Double]],
        Vector[Countable[Item, Double]]
    ) =
      solution.inputs
        .filter( _.amount.abs > 1e-12 )
        .foldMap(
          input =>
            recipeSelection.extractionRecipes
              .get( input.item )
              .flatMap(
                r =>
                  r.productsPerMinute
                    .find( _.item == input.item )
                    .map( p => Countable( r, input.amount / p.amount ) )
              )
              .cata(
                r => ( Vector( r ), Vector.empty ),
                ( Vector.empty, Vector( input ) )
              )
        )

    val sortedBlocks: Vector[FactoryBlock] =
      ( Vector.empty[Countable[Recipe[Machine, Item], Double]], ( inputRecipes, solution.recipes ) )
        .tailRecM[Id, Vector[FactoryBlock]] {
          case ( acc, ( available, rest ) ) =>
            if (available.isEmpty)
              Right( (acc ++ rest).map( FactoryBlock( _ ) ) )
            else {
              val next = acc ++ available
              Left(
                (
                  next,
                  rest.partition(
                    recipe =>
                      recipe.item.ingredients.forall( it => next.exists( _.item.product.exists( _.item == it.item ) ) )
                  )
                )
              )
            }
        }

    Factory( bill, sortedBlocks, extraInputs )
  }

}
