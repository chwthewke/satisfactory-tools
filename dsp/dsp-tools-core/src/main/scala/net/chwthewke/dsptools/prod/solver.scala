package net.chwthewke.dsptools
package prod

import cats.Order
import cats.Show
import cats.data.NonEmptyVector
import cats.derived.semiauto
import scala.concurrent.duration._
import spire.compat._
import spire.math.Rational

import net.chwthewke.factory.data.Countable
import net.chwthewke.factory.prod.SolverModule
import gamedata.RecipeType
import model.Item
import model.Productivity
import model.Recipe

object solver extends SolverModule {
  override type ItemId = Int
  override type ItemT  = Item

  override protected def itemId( item: Item ): Int = item.id

  sealed abstract class ModifiedRecipe {
    def baseRecipe: Recipe
    def id: String
    def ingredients: Vector[Countable[Rational, Item]]
    def products: NonEmptyVector[Countable[Rational, Item]]
    def itemsPerMinute: Vector[Countable[Double, Item]]
    def powerMultiplier: Double

    def displayName: String
    def recipeType: RecipeType = baseRecipe.recipeType

    def ingredientsPerMinute: Vector[Countable[Double, Item]] =
      ingredients.map( perMinute )
    def productsPerMinute: NonEmptyVector[Countable[Double, Item]] =
      products.map( perMinute )

    protected def perMinute( ct: Countable[Rational, Item] ): Countable[Double, Item] =
      ct.mapAmount( am => am.toDouble * (1.minute / baseRecipe.duration) )
  }

  case class SimpleRecipe( baseRecipe: Recipe ) extends ModifiedRecipe {
    override def id: String                                          = baseRecipe.id.toString
    override def displayName: String                                 = baseRecipe.displayName
    override def ingredients: Vector[Countable[Rational, Item]]      = baseRecipe.ingredients
    override def products: NonEmptyVector[Countable[Rational, Item]] = baseRecipe.products
    override def itemsPerMinute: Vector[Countable[Double, Item]]     = baseRecipe.itemsPerMinute
    override val powerMultiplier: Double                             = 1d
  }

  case class ProductiveRecipe( baseRecipe: Recipe, productivityItem: Item ) extends ModifiedRecipe {
    override def id: String = s"P_${baseRecipe.id}_${productivityItem.id}"

    override def displayName: String = s"${baseRecipe.displayName} [P${productivityItem.productivityLevel}]"
    val productivity: Productivity   = Productivity( productivityItem.productivityLevel )

    override val ingredients: Vector[Countable[Rational, Item]] = {
      val reqProductivityItems: Rational = baseRecipe.ingredients
        .map( _.amount )
        .sum / productivityItem.productivityUses

      (baseRecipe.ingredients :+ Countable( productivityItem, reqProductivityItems )).gather
    }

    override val products: NonEmptyVector[Countable[Rational, Item]] =
      baseRecipe.products.map( _.mapAmount( _ * productivity.productIncrease ) )

    override val itemsPerMinute: Vector[Countable[Double, Item]] =
      (ingredients.map( _.mapAmount( -_ ) ) ++ products.toVector).gather
        .map( _.mapAmount( am => am.toDouble * (1.minute / baseRecipe.duration) ) )

    override def powerMultiplier: Double = Productivity( productivityItem.productivityLevel ).powerIncrease
  }

  object ModifiedRecipe {
    implicit val modifiedRecipeShow: Show[ModifiedRecipe] = semiauto.show[ModifiedRecipe]
    implicit val modifiedRecipeOrder: Order[ModifiedRecipe] =
      Order.by[ModifiedRecipe, ( Recipe, Option[Int] )] {
        case SimpleRecipe( baseRecipe ) => ( baseRecipe, None )
        case ProductiveRecipe( baseRecipe, productivityItem ) =>
          ( baseRecipe, Some( productivityItem.productivityLevel ) )
      }
  }

  override type RecipeId = String
  override type RecipeT  = ModifiedRecipe

  override protected def recipeId( recipe: ModifiedRecipe ): String = recipe.id
  override protected def itemsPerMinute( recipe: ModifiedRecipe ): Vector[Countable[Double, Item]] =
    recipe.itemsPerMinute

  object Solver extends GenericSolver
}
