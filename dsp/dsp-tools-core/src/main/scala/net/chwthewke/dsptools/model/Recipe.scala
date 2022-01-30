package net.chwthewke.dsptools
package model

import cats.Monoid
import cats.Order
import cats.Show
import cats.data.NonEmptyVector
import cats.syntax.foldable._
import cats.syntax.semigroup._
import cats.syntax.show._
import scala.concurrent.duration._
import spire.math.Rational

import net.chwthewke.factory.data.Countable
import gamedata.RecipeProto
import gamedata.RecipeType

case class Recipe(
    id: Int,
    displayName: String,
    ingredients: Vector[Countable[Rational, Item]],
    products: NonEmptyVector[Countable[Rational, Item]],
    recipeType: RecipeType,
    duration: FiniteDuration,
    recipeProductive: Boolean
) {

  def productive: Boolean = recipeProductive && ingredients.forall( _.item.productive )

  private implicit val rationalMonoid: Monoid[Rational] = Rational.RationalAlgebra.additive

  def itemsMap: Map[Item, Rational] =
    ingredients.foldMap { case Countable( item, amount ) => Map( item -> -amount ) } |+|
      products.foldMap { case Countable( item, amount )  => Map( item -> amount ) }

  def itemsPerMinute: Vector[Countable[Double, Item]] =
    itemsMap.map { case ( item, amount ) => Countable( item, amount ) }.map( perMinute ).toVector

  def ingredientsPerMinute: Vector[Countable[Double, Item]] =
    ingredients.map( perMinute )

  def productsPerMinute: NonEmptyVector[Countable[Double, Item]] =
    products.map( perMinute )

  private def perMinute( ct: Countable[Rational, Item] ): Countable[Double, Item] =
    ct.mapAmount( am => am.toDouble * (1.minute / duration) )
}

object Recipe {
  def apply(
      displayName: String,
      ingredients: Vector[Countable[Int, Item]],
      products: NonEmptyVector[Countable[Int, Item]],
      proto: RecipeProto
  ): Recipe = {
    val duration: FiniteDuration = (proto.timeSpend * 1000 / 60d).millis // orig. in 60 UPS ticks
    Recipe(
      proto.id,
      displayName,
      ingredients.map( _.mapAmount( n => Rational( n ) ) ),
      products.map( _.mapAmount( n => Rational( n ) ) ),
      proto.recipeType,
      duration,
      !proto.nonProductive
    )
  }

  private implicit val rationalShow: Show[Rational] = Show.fromToString

  implicit val recipeShow: Show[Recipe] =
    Show.show {
      case recipe =>
        show"""  ${recipe.displayName} #${recipe.id}
              |  Ingredients:
              |    ${recipe.ingredients.map( _.map( _.displayName ).show ).intercalate( "\n    " )}
              |  Products:
              |    ${recipe.products.map( _.map( _.displayName ).show ).intercalate( "\n    " )}
              |  Duration: ${recipe.duration}
              |  Productive: ${recipe.productive}
              |""".stripMargin
    }

  implicit val recipeOrder: Order[Recipe] = Order.by( _.id )
}
