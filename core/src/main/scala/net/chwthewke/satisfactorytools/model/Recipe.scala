package net.chwthewke.satisfactorytools
package model

import cats.Show
import cats.data.NonEmptyList
import cats.syntax.foldable._
import cats.syntax.semigroup._
import cats.syntax.show._
import scala.concurrent.duration._

import data.ClassName
import data.Countable
import data.Item

final case class Recipe(
    className: ClassName,
    displayName: String,
    ingredients: List[Countable[Double, Item]],
    products: NonEmptyList[Countable[Double, Item]],
    duration: FiniteDuration,
    producedIn: Machine,
    power: Power
) {
  def ingredientsPerMinute: List[Countable[Double, Item]]      = ingredients.map( perMinute )
  def productsPerMinute: NonEmptyList[Countable[Double, Item]] = products.map( perMinute )

  def itemsPerMinuteMap: Map[Item, Double] =
    productsPerMinute.foldMap { case Countable( it, am )      => Map( it -> am ) } |+|
      ingredientsPerMinute.foldMap { case Countable( it, am ) => Map( it -> -am ) }

  def itemsPerMinute: Vector[Countable[Double, Item]] =
    itemsPerMinuteMap.map { case ( item, amount ) => Countable( item, amount ) }.toVector

  def isExtraction: Boolean =
    producedIn.machineType.isExtractor

  private def perMinute( ct: Countable[Double, Item] ): Countable[Double, Item] =
    Countable( ct.item, ct.amount * 60000 / duration.toMillis )

  def isAlternate: Boolean = displayName.toLowerCase.startsWith( "alternate" )

}

object Recipe {

  implicit def recipeShow( implicit showItem: Show[Item], showMachine: Show[Machine] ): Show[Recipe] =
    Show.show {
      case Recipe( className, displayName, ingredients, products, duration, producer, power ) =>
        show"""  $displayName # $className
              |  Ingredients:
              |    ${ingredients.map( _.show ).intercalate( "\n    " )}
              |  Products:
              |    ${products.map( _.show ).intercalate( "\n    " )}
              |  Duration $duration
              |  Producer:
              |    $producer
              |""".stripMargin
    }

}
