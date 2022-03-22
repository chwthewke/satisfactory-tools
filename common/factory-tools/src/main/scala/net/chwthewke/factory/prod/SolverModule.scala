package net.chwthewke.factory
package prod

import cats.Show
import cats.syntax.foldable._
import cats.syntax.traverse._
import cats.syntax.show._
import org.ojalgo.optimisation.ExpressionsBasedModel
import org.ojalgo.optimisation.Variable
import mouse.any._
import mouse.option._
import org.ojalgo.optimisation.Expression

import data.Countable
import net.chwthewke.factory.prod.Direction.Provide
import net.chwthewke.factory.prod.Direction.Receive

trait SolverModule {
  type ItemId
  type ItemT
  protected def itemId( item: ItemT ): ItemId

  type RecipeId
  type RecipeT
  protected def recipeId( recipe: RecipeT ): RecipeId
  protected def itemsPerMinute( recipe: RecipeT ): Vector[Countable[Double, ItemT]]

  case class Solution(
      recipes: Vector[Countable[Double, RecipeT]],
      inputs: Vector[Countable[Double, ItemT]],
      flows: Vector[( ItemT, Vector[( Direction, RecipeT, Double )] )]
  )

  class GenericSolver( implicit itemIdShow: Show[ItemId], recipeIdShow: Show[RecipeId] ) {

    private def inputVar(
        model: ExpressionsBasedModel,
        item: ItemT,
        weight: Double,
        cap: Option[Double]
    ): Variable = {
      val setUpper = (v: Variable) => cap.cata( v.upper, v )

      model.addVariable( show"I__${itemId( item )}" ).weight( weight ).lower( 0d ) |> setUpper
    }

    private def recipeVar( model: ExpressionsBasedModel, recipe: RecipeT, weight: Double ): Variable =
      model.addVariable( show"R__${recipeId( recipe )}" ).weight( weight ).lower( 0d )

    private def itemExpr(
        model: ExpressionsBasedModel,
        item: ItemT,
        itemVar: Option[Variable],
        requested: Option[Double],
        recipes: Vector[( Variable, Double )]
    ): Expression = {
      val target = model
        .addExpression( show"X__${itemId( item )}" )
        .lower( requested.getOrElse( 0d ) )

      val withInput = itemVar.cata( target.set( _, 1d ), target )

      recipes.foldl( withInput ) { case ( expr, ( recipeVar, amount ) ) => expr.set( recipeVar, amount ) }
    }

    def solve(
        request: Vector[Countable[Double, ItemT]],
        recipes: Vector[( RecipeT, Double )],
        inputWeights: Map[ItemT, Double],
        inputCaps: Map[ItemT, Double]
    ): Either[String, Solution] = {

      val model: ExpressionsBasedModel = new ExpressionsBasedModel

      val recipeVars: Map[RecipeT, Variable] =
        recipes.map { case ( recipe, weight ) => ( recipe, recipeVar( model, recipe, weight ) ) }.toMap

      val inputVars: Map[ItemT, Variable] =
        inputWeights.map { case ( item, weight ) => ( item, inputVar( model, item, weight, inputCaps.get( item ) ) ) }

      /*val itemExprs: Map[ItemT, Expression] =*/
      recipes
        .foldMap {
          case ( recipe, _ ) =>
            itemsPerMinute( recipe ).foldMap {
              case Countable( item, amount ) =>
                Map( item -> Vector( ( recipe, amount ) ) )
            }
        }
        .map {
          case ( item, recipes ) =>
            (
              item,
              itemExpr(
                model,
                item,
                inputVars.get( item ),
                request.find( _.item == item ).map( _.amount ),
                recipes.map { case ( recipe, amount ) => ( recipeVars( recipe ), amount ) }
              )
            )
        }

      val result = model.minimise()

//      println( model )

      Option
        .when( result.getState.isSuccess ) {
          val recipes: Vector[Countable[Double, RecipeT]] =
            recipeVars.flatMap {
              case ( recipe, v ) =>
                val amount = v.getValue.doubleValue
                Option.when( amount != 0d )( Countable( recipe, amount ) )
            }.toVector

          val inputs: Vector[Countable[Double, ItemT]] =
            inputVars.map { case ( item, v ) => Countable( item, v.getValue.doubleValue ) }.toVector

          val flows: Vector[( ItemT, Vector[( Direction, RecipeT, Double )] )] =
            recipes
              .foldMap { recipe =>
                recipe.flatTraverse( itemsPerMinute ).foldMap {
                  case Countable( item, amount ) =>
                    Map( item -> Map( ( recipe.item, if (amount < 0) Provide else Receive ) -> amount ) )
                }
              }
              .map {
                case ( item, byRecipe ) => ( item, byRecipe.map { case ( ( r, d ), a ) => ( d, r, a ) }.toVector )
              }
              .toVector

          Solution(
            recipes,
            inputs,
            flows
          )
        }
        .toRight( s"Solver state: ${result.getState}" )
    }

  }

}
