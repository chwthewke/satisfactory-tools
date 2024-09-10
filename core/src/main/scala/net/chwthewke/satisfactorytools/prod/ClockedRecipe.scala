package net.chwthewke.satisfactorytools
package prod

import cats.data.NonEmptyList
import cats.syntax.flatMap._
import cats.syntax.traverse._

import data.Countable
import data.Item
import model.Machine
import model.Power
import model.Recipe

/**
 * Represents a while number of machines producing a recipe at a clock speed allowing for a given target production
 *
 *   - For extraction recipes, the number and clock speed are computed at once, going through the available resource
 *     nodes in order of decreasing purity
 *
 *   - For manufacturing recipes, calculated from the required amount.
 *
 * @param recipe
 *   the fractional amount of machines producing the recipe
 * @param clockSpeed
 *   the clock speed of the machines
 * @param machineCount
 *   the integer amount of machines
 */
case class ClockedRecipe(
    recipe: Countable[Double, Recipe],
    clockSpeed: Double,
    machineCount: Int
) {

  def machine: Machine                 = recipe.item.producedIn
  def powerConsumptionExponent: Double = machine.powerConsumptionExponent

  val clockSpeedMillionth: Int = ( clockSpeed * 10000d ).ceil.toInt

  def fractionalAmount: Double = recipe.amount

  def power: Power =
    recipe.item.power.map( _ * machineCount * math.pow( clockSpeedMillionth / 1e6d, powerConsumptionExponent ) )

  val mainProductAmount: Double        = recipe.flatMap( _.productsPerMinute.head ).amount
  val mainProductAmountPerUnit: Double = mainProductAmount / machineCount

  val ingredientsPerMinute: List[Countable[Double, Item]] = recipe.flatTraverse( _.ingredientsPerMinute )

  val productsPerMinute: NonEmptyList[Countable[Double, Item]] = recipe.flatTraverse( _.productsPerMinute )

  def multipleOf( n: Int ): ClockedRecipe =
    ClockedRecipe.fixed( recipe.item, fractionalAmount, 1 + n * ( ( machineCount - 1 ) / n ) )
}

object ClockedRecipe {
  def fixed( recipe: Recipe, fractionalAmount: Double, amount: Int ): ClockedRecipe =
    ClockedRecipe( Countable( recipe, fractionalAmount ), fractionalAmount / amount * 100d, amount )

  def roundUp( recipe: Countable[Double, Recipe] ): ClockedRecipe =
    fixed( recipe.item, recipe.amount, recipe.amount.ceil.toInt )

  def overclocked( recipe: Countable[Int, Recipe], clockSpeed: Double ): ClockedRecipe =
    ClockedRecipe( Countable( recipe.item, recipe.amount.toDouble * clockSpeed / 100d ), clockSpeed, recipe.amount )
}
