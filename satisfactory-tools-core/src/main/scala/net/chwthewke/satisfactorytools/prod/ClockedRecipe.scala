package net.chwthewke.satisfactorytools
package prod

import cats.Functor
import cats.data.NonEmptyList
import cats.syntax.functor._

import data.Countable
import data.Item
import model.Machine
import model.Power
import model.Recipe

/**
  * Represents a while number of machines producing a recipe at a clock speed allowing for a given target production
  *
  * - For extraction recipes, the number and clock speed are computed at once, going through the available resource nodes
  * in order of decreasing purity
  *
  * - For manufacturing recipes, calculated from the required amount.
  *
  * @param recipe the amount of machines producing the recipe
  * @param clockSpeed the clock speed of the machines
  */
case class ClockedRecipe(
    recipe: Countable[Int, Recipe],
    clockSpeed: Double,
    fractionalAmount: Double
) {

  val machineCount: Int        = recipe.amount
  val clockSpeedMillionth: Int = (clockSpeed * 10000d).ceil.toInt

  val machine: Machine = recipe.item.producedIn

  def power: Power = recipe.item.power.map( _ * machineCount * math.pow( clockSpeedMillionth / 1e6d, 1.6d ) )

  val itemAmount: Double        = recipe.amount * recipe.item.productsPerMinute.head.amount * clockSpeed / 100d
  val itemAmountPerUnit: Double = itemAmount / machineCount

  private def withAmount[F[_]: Functor]( items: F[Countable[Double, Item]] ): F[Countable[Double, Item]] =
    items.map { case Countable( item, amount ) => Countable( item, amount * fractionalAmount ) }

  val ingredients: List[Countable[Double, Item]]          = withAmount( recipe.item.ingredients )
  val ingredientsPerMinute: List[Countable[Double, Item]] = withAmount( recipe.item.ingredientsPerMinute )

  val products: NonEmptyList[Countable[Double, Item]]          = withAmount( recipe.item.products )
  val productsPerMinute: NonEmptyList[Countable[Double, Item]] = withAmount( recipe.item.productsPerMinute )
}

object ClockedRecipe {
  def roundUp( recipe: Countable[Double, Recipe] ): ClockedRecipe = {
    val machineCount: Int  = recipe.amount.ceil.toInt
    val clockSpeed: Double = recipe.amount / machineCount * 100d

    ClockedRecipe( Countable( recipe.item, machineCount ), clockSpeed, recipe.amount )
  }

  def overclocked( recipe: Countable[Int, Recipe], clockSpeed: Double ): ClockedRecipe =
    ClockedRecipe( recipe, clockSpeed, recipe.amount.toDouble * clockSpeed / 100d )
}
