package net.chwthewke.satisfactorytools
package model

import cats.Applicative
import cats.Show
import cats.data.NonEmptyList
import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.option._
import cats.syntax.show._
import cats.syntax.traverse._
import io.circe.Decoder
import scala.concurrent.duration._
//

final case class Recipe[M, N](
    className: ClassName,
    displayName: String,
    ingredients: List[Countable[N, Double]],
    product: NonEmptyList[Countable[N, Double]],
    duration: FiniteDuration,
    producers: List[M]
) {
  def ingredientsPerMinute: List[Countable[N, Double]]      = ingredients.map( perMinute )
  def productsPerMinute: NonEmptyList[Countable[N, Double]] = product.map( perMinute )

  def isExtraction( implicit ev: M =:= Machine ): Boolean =
    producers.forall( p => ev( p ).machineType == MachineType.Extractor )

  private def perMinute( ct: Countable[N, Double] ): Countable[N, Double] =
    Countable( ct.item, ct.amount * 60000 / duration.toMillis )

  def traverseIngredientsAndProducts[F[_]: Applicative, P](
      f: Countable[N, Double] => F[Countable[P, Double]]
  ): F[Recipe[M, P]] =
    ( ingredients.traverse( f ), product.traverse( f ) )
      .mapN( ( ing, prd ) => copy[M, P]( ingredients = ing, product = prd ) )
}

object Recipe {

  implicit val recipeDecoder: Decoder[Recipe[ClassName, ClassName]] = {
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
          in: List[Countable[ClassName, Double]],
          out: NonEmptyList[Countable[ClassName, Double]],
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
              |    ${producers.map( _.show ).intercalate( "\n    " )}
              |""".stripMargin
    }

}
