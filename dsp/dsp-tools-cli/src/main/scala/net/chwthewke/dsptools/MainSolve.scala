package net.chwthewke.dsptools

import cats.Semigroup
import cats.data.NonEmptyVector
import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.functor._
import scala.concurrent.duration._

import net.chwthewke.factory.data.Countable
import net.chwthewke.factory.prod.Direction
import gamedata.RecipeType
import model.Item
import model.Model
import model.Recipe
import prod.solver.ModifiedRecipe
import prod.solver.ProductiveRecipe
import prod.solver.SimpleRecipe
import prod.solver.Solution
import prod.solver.Solver

object MainSolve {

  case class ProdItem( item: Item ) extends AnyVal
  object ProdItem {
    implicit val prodItemSemigroup: Semigroup[ProdItem] =
      ( x, y ) => Vector( x, y ).minBy( _.item.productivityLevel )
  }

  def recipeWeight( recipe: Recipe ): Double =
    recipe.recipeType match {
      case RecipeType.None        => 10d
      case RecipeType.Smelt       => 0.36d
      case RecipeType.Chemical    => 0.72d
      case RecipeType.Refine      => 0.96d
      case RecipeType.Assemble    => 0.54d
      case RecipeType.Particle    => 12d
      case RecipeType.Exchange    => 45d
      case RecipeType.PhotonStore => 200d
      case RecipeType.Fractionate => 0.72d
      case RecipeType.Research    => 0.48d
    }

  def productiveRecipes(
      weightedRecipe: ( Recipe, Double ),
      productivityItems: Vector[Item],
      maxProd: Int
  ): Vector[( ModifiedRecipe, Double )] = {
    val recipe = weightedRecipe._1
    val weight = weightedRecipe._2

    ( SimpleRecipe( recipe ), weight ) +: productivityItems
      .filter(
        item => item.productivityLevel > 0 && item.productivityLevel <= maxProd && recipe.productive
      )
      .map { item =>
        val productiveRecipe = ProductiveRecipe( recipe, item )
        ( productiveRecipe, weight * productiveRecipe.powerMultiplier )
      }
  }

  def solve(
      model: Model,
      plan: Plan
  ): Either[String, ( Vector[Countable[Double, Item]], Solution )] = {

    val request: Vector[Countable[Double, Item]] =
      plan.bill
        .flatMap {
          case ( itemId, amount ) =>
            model.items.find( _.id == itemId ).map( Countable( _, amount ) )
        }

    val chargeAcc: Option[Recipe] =
      (
        model.items.find( _.id == 2206 ),
        model.items.find( _.id == 2207 )
      ).mapN(
        ( acc, accFull ) =>
          Recipe(
            201,
            "Charge accumulator",
            Vector( Countable( acc, 1 ) ),
            NonEmptyVector.of( Countable( accFull, 1 ) ),
            RecipeType.Exchange,
            6.seconds,
            recipeProductive = false
          )
      )

    val allowedRecipes: Vector[( ModifiedRecipe, Double )] =
      (
        plan.recipes
          .flatMap( id => model.recipes.find( _.id == id ) )
          ++ chargeAcc
      ).fproduct( recipeWeight )
        .flatMap( productiveRecipes( _, model.items, plan.maxProductivity ) )

    val allowedInputs: Map[Item, Double] =
      plan.inputs.flatMap {
        case ( id, weight ) =>
          model.items.find( _.id == id ).tupleRight( weight.toDouble )
      }.toMap

    def resolveSprayIntermediates( solution: Solution ): Either[String, Solution] = {

      lazy val neededSprays =
        solution.flows.foldMap {
          case ( _, flows ) =>
            val ( amt, prod ) = flows
              .filter( _._1 == Direction.Provide )
              .foldMap {
                case ( _, recipe, amount ) =>
                  recipe match {
                    case SimpleRecipe( _ ) => ( amount.abs, None )
                    case ProductiveRecipe( _, productivityItem ) =>
                      ( 0d, Some( ProdItem( productivityItem ) ) )
                  }
              }
            prod.map( p => Countable( p.item, amt / p.item.productivityUses ) ).toVector
        }.gather

      if (!plan.sprayIntermediates || neededSprays.isEmpty) Right( solution )
      else
        Solver.solve(
          (request ++ neededSprays).gather,
          allowedRecipes,
          allowedInputs,
          Map.empty
        )
    }

    Solver
      .solve(
        request,
        allowedRecipes,
        allowedInputs,
        Map.empty
      )
      .flatMap( resolveSprayIntermediates )
      .tupleLeft( request )

  }

}
