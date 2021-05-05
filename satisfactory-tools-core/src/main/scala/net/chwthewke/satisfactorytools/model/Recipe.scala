package net.chwthewke.satisfactorytools
package model

import cats.Applicative
import cats.Show
import cats.data.NonEmptyList
import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.option._
import cats.syntax.semigroup._
import cats.syntax.show._
import cats.syntax.traverse._
import io.circe.Decoder
import scala.concurrent.duration._

final case class Recipe[M, N](
    className: ClassName,
    displayName: String,
    ingredients: List[Countable[Double, N]], // TODO can be NEL?
    products: NonEmptyList[Countable[Double, N]],
    duration: FiniteDuration,
    producedIn: M
) {
  def ingredientsPerMinute: List[Countable[Double, N]]      = ingredients.map( perMinute )
  def productsPerMinute: NonEmptyList[Countable[Double, N]] = products.map( perMinute )

  def itemsPerMinuteMap: Map[N, Double] =
    productsPerMinute.foldMap { case Countable( it, am )      => Map( it -> am ) } |+|
      ingredientsPerMinute.foldMap { case Countable( it, am ) => Map( it -> -am ) }

  def itemsPerMinute: Vector[Countable[Double, N]] =
    itemsPerMinuteMap.map { case ( item, amount ) => Countable( item, amount ) }.toVector

  def isExtraction( implicit ev: M =:= Machine ): Boolean =
    producedIn.machineType.isExtractor

  private def perMinute( ct: Countable[Double, N] ): Countable[Double, N] =
    Countable( ct.item, ct.amount * 60000 / duration.toMillis )

  def isAlternate: Boolean = displayName.toLowerCase.startsWith( "alternate" )

  def traverseIngredientsAndProducts[F[_]: Applicative, P](
      f: Countable[Double, N] => F[Countable[Double, P]]
  ): F[Recipe[M, P]] =
    ( ingredients.traverse( f ), products.traverse( f ) )
      .mapN( ( ing, prd ) => copy[M, P]( ingredients = ing, products = prd ) )
}

object Recipe {

  implicit val recipeDecoder: Decoder[Recipe[List[ClassName], ClassName]] = {
    import Parsers._

    Decoder.forProduct6(
      "ClassName",
      "mDisplayName",
      "mIngredients",
      "mProduct",
      "mManufactoringDuration",
      "mProducedIn"
    )(
      (
          cn: ClassName,
          dn: String,
          in: List[Countable[Double, ClassName]],
          out: NonEmptyList[Countable[Double, ClassName]],
          dur: FiniteDuration,
          mch: List[ClassName]
      ) => Recipe( cn, dn, in, out, dur, mch )
    )(
      Decoder[ClassName],
      Decoder[String],
      countableList.decoder.map( _.toList ),
      countableList.decoder,
      Decoders.doubleStringDecoder.map( _.seconds ),
      Decoder.decodeOption( buildablesList.decoder ).map( _.orEmpty )
    )
  }

  implicit def recipeShow[M: Show, N: Show]: Show[Recipe[M, N]] =
    Show.show {
      case Recipe( className, displayName, ingredients, products, duration, producers ) =>
        show"""  $displayName # $className
              |  Ingredients: 
              |    ${ingredients.map( _.show ).intercalate( "\n    " )}
              |  Products:
              |    ${products.map( _.show ).intercalate( "\n    " )}
              |  Duration $duration
              |  Producers:
              |    $producers
              |""".stripMargin
    }

}
