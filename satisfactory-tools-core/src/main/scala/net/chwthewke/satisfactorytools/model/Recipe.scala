package net.chwthewke.satisfactorytools
package model

import cats.Applicative
import cats.Eval
import cats.Foldable
import cats.Show
import cats.Traverse
import cats.data.NonEmptyList
import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.show._
import cats.syntax.traverse._
import io.circe.Decoder
import scala.concurrent.duration._
//

final case class Recipe[M, N](
    className: ClassName,
    displayName: String,
    ingredients: List[Countable[N, Int]],
    product: NonEmptyList[Countable[N, Int]],
    duration: FiniteDuration,
    producers: List[M]
) {
  def ingredientsPerMinute: List[Countable[N, Double]]      = ingredients.map( perMinute )
  def productsPerMinute: NonEmptyList[Countable[N, Double]] = product.map( perMinute )

  def isExtraction( implicit ev: M =:= Machine ): Boolean =
    producers.forall( p => ev( p ).machineType == MachineType.Extractor )

  private def perMinute( ct: Countable[N, Int] ): Countable[N, Double] =
    Countable( ct.item, ct.amount.toDouble * 60000 / duration.toMillis )

}

object Recipe {

  implicit def recipeTraverse[M]: Traverse[Recipe[M, *]] = new Traverse[Recipe[M, *]] {
    private def itemList[A]( fa: Recipe[M, A] ): List[A] =
      fa.ingredients.map( _.item ) ++ fa.product.toList.map( _.item )

    override def traverse[G[_], A, B](
        fa: Recipe[M, A]
    )( f: A => G[B] )( implicit A: Applicative[G] ): G[Recipe[M, B]] =
      (
        fa.ingredients.traverse( i => f( i.item ).map( Countable( _, i.amount ) ) ),
        fa.product.traverse( i => f( i.item ).map( Countable( _, i.amount ) ) )
      ).mapN( ( in, out ) => Recipe( fa.className, fa.displayName, in, out, fa.duration, fa.producers ) )

    override def foldLeft[A, B]( fa: Recipe[M, A], b: B )( f: ( B, A ) => B ): B =
      Foldable[List].foldLeft( itemList( fa ), b )( f )

    override def foldRight[A, B]( fa: Recipe[M, A], lb: Eval[B] )( f: ( A, Eval[B] ) => Eval[B] ): Eval[B] =
      Foldable[List].foldRight( itemList( fa ), lb )( f )

  }

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
          in: List[Countable[ClassName, Int]],
          out: NonEmptyList[Countable[ClassName, Int]],
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
