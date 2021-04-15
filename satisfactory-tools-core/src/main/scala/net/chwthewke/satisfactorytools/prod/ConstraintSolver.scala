package net.chwthewke.satisfactorytools
package prod

import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.show._
import mouse.any._
import org.ojalgo.optimisation.Expression
import org.ojalgo.optimisation.ExpressionsBasedModel
import org.ojalgo.optimisation.Variable

import model.Countable
import model.Item
import model.Machine
import model.Recipe

object ConstraintSolver extends Solver {

  private def varName( item: Item ): String                    = show"I__${item.className}"
  private def varName( recipe: Recipe[Machine, Item] ): String = show"R__${recipe.className}"
  private def exprName( item: Item ): String                   = show"X__${item.className}"

  override def solve( bill: Bill, recipes: RecipeSelection ): Either[String, Solution] = {

    val actualRecipes = recipes.allowedRecipes

    val model = new ExpressionsBasedModel

    val recipeVars: Map[Recipe[Machine, Item], Variable] =
      actualRecipes.fproduct( recipe => model.addVariable( varName( recipe ) ).lower( 0d ) ).toMap

    def inputVar( item: Item ): Variable = {
      val setUpper  = (v: Variable) => recipes.resourceCaps.get( item ).fold( v )( v.upper )
      val setWeight = (v: Variable) => recipes.resourceWeights.get( item ).fold( v )( v.weight )

      model.addVariable( varName( item ) ).lower( 0d ) |> setUpper |> setWeight
    }

    val inputVars: Map[Item, Variable] =
      recipes.extractionRecipes.map { case ( item, _ ) => ( item, inputVar( item ) ) }

    val itemExprs: Map[Item, Expression] =
      actualRecipes
        .foldMap( _.reducedItemsPerMinute.keySet )
        .toVector
        .fproduct(
          item =>
            model
              .addExpression( exprName( item ) )
              .lower( bill.items.find( _.item == item ).fold( 0d )( _.amount ) )
        )
        .toMap

    actualRecipes.foreach(
      recipe =>
        recipe.reducedItemsPerMinute.foreach {
          case ( item, amount ) =>
            itemExprs( item ).set( recipeVars( recipe ), amount )
        }
    )

    inputVars.foreach { case ( item, inputVar ) => itemExprs.get( item ).foreach( _.set( inputVar, 1d ) ) }

    val result = model.minimise()

    Option
      .when( result.getState.isSuccess )(
        Solution(
          recipeVars.flatMap {
            case ( recipe, v ) =>
              val amount = v.getValue.doubleValue
              Option.when( amount != 0d )( Countable( recipe, amount ) )
          }.toVector,
          inputVars.map { case ( item, v ) => Countable( item, v.getValue.doubleValue ) }.toVector
        )
      )
      .toRight( s"Solver state: ${result.getState}" )
  }

}
