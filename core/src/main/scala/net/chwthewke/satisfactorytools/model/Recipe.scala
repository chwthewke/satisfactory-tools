package net.chwthewke.satisfactorytools
package model

import cats.Show
import cats.data.NonEmptyList
import cats.syntax.all._
import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder
import scala.concurrent.duration._

import data.ClassName
import data.Countable
import data.Item

final case class Recipe(
    className: ClassName,
    displayName: String,
    category: RecipeCategory,
    ingredients: List[Countable[Double, Item]],
    products: NonEmptyList[Countable[Double, Item]],
    duration: FiniteDuration,
    producedIn: Machine,
    power: Power
) {
  def ingredientsPerMinute: List[Countable[Double, Item]]      = ingredients.map( perMinute )
  def productsPerMinute: NonEmptyList[Countable[Double, Item]] = products.map( perMinute )

  def itemsPerMinuteMap: Map[Item, Double] =
    productsPerMinute.foldMap { case Countable( it, am ) => Map( it -> am ) } |+|
      ingredientsPerMinute.foldMap { case Countable( it, am ) => Map( it -> -am ) }

  def itemsPerMinute: Vector[Countable[Double, Item]] =
    itemsPerMinuteMap.map { case ( item, amount ) => Countable( item, amount ) }.toVector

  def isExtraction: Boolean =
    producedIn.machineType.isExtractor

  private def perMinute( ct: Countable[Double, Item] ): Countable[Double, Item] =
    Countable( ct.item, ct.amount * 60000 / duration.toMillis )

  def isAlternate: Boolean = displayName.toLowerCase.startsWith( "alternate" )

  // NOTE iffy, but that's what we have
  def isMatterConversion: Boolean =
    producedIn.className == ClassName( "Build_Converter_C" ) &&
      ingredients.size == 2 &&
      products.size == 1 &&
      ingredients.exists( _.item.className == ClassName( "Desc_SAMIngot_C" ) ) &&
      products.head.item.className != ClassName( "Desc_FicsiteIngot_C" )
}

object Recipe {

  implicit val recipeShow: Show[Recipe] =
    Show.show {
      case Recipe( className, displayName, category, ingredients, products, duration, producer, power ) =>
        show"""  $displayName # $className ${category.tierOpt.map( t => s"(tier $t)" ).orEmpty}
              |  Ingredients:
              |    ${ingredients.map( _.map( _.displayName ).show ).intercalate( "\n    " )}
              |  Products:
              |    ${products.map( _.map( _.displayName ).show ).intercalate( "\n    " )}
              |  Duration: $duration
              |  Power: $power 
              |  Produced in: ${producer.displayName}
              |""".stripMargin
    }

  import scala.concurrent.duration._
  private implicit val finiteDurationDecoder: Decoder[FiniteDuration] = Decoder[Long].map( _.millis )
  private implicit val finiteDurationEncoder: Encoder[FiniteDuration] = Encoder[Long].contramap( _.toMillis )

  implicit val recipeDecoder: Decoder[Recipe] = deriveDecoder[Recipe]
  implicit val recipeEncoder: Encoder[Recipe] = deriveEncoder[Recipe]
}
