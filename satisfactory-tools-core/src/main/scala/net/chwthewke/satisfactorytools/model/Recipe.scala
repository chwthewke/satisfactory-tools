package net.chwthewke.satisfactorytools.model

import cats.Applicative
import cats.Eval
import cats.Foldable
import cats.Show
import cats.Traverse
import cats.data.NonEmptyList
import cats.instances.list._
import cats.instances.string._
import cats.instances.finiteDuration._
import cats.syntax.apply._
import cats.syntax.functor._
import cats.syntax.show._
import cats.syntax.traverse._
import io.circe.Decoder
import scala.concurrent.duration._
//

final case class Recipe[N](
    className: ClassName,
    displayName: String,
    ingredients: List[Countable[N]],
    product: NonEmptyList[Countable[N]],
    duration: FiniteDuration,
    producers: List[N]
)

object Recipe {

  implicit val recipeTraverse: Traverse[Recipe] = new Traverse[Recipe] {
    private def itemList[A]( fa: Recipe[A] ): List[A] =
      fa.ingredients.map( _.item ) ++ fa.product.toList.map( _.item ) ++ fa.producers

    override def traverse[G[_], A, B]( fa: Recipe[A] )( f: A => G[B] )( implicit A: Applicative[G] ): G[Recipe[B]] =
      (
        fa.ingredients.traverse( i => f( i.item ).map( Countable( _, i.amount ) ) ),
        fa.product.traverse( i => f( i.item ).map( Countable( _, i.amount ) ) ),
        fa.producers.traverse( f )
      ).mapN( ( in, out, bds ) => Recipe( fa.className, fa.displayName, in, out, fa.duration, bds ) )

    override def foldLeft[A, B]( fa: Recipe[A], b: B )( f: ( B, A ) => B ): B =
      Foldable[List].foldLeft( itemList( fa ), b )( f )

    override def foldRight[A, B]( fa: Recipe[A], lb: Eval[B] )( f: ( A, Eval[B] ) => Eval[B] ): Eval[B] =
      Foldable[List].foldRight( itemList( fa ), lb )( f )

  }

  implicit val recipeDecoder: Decoder[Recipe[ClassName]] = {
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
          in: List[Countable[ClassName]],
          out: NonEmptyList[Countable[ClassName]],
          dur: FiniteDuration,
          mch: List[ClassName]
      ) => Recipe( cn, dn, in, out, dur, mch )
    )(
      Decoder[ClassName],
      Decoder[String],
      countableList.decoder.map( _.toList ),
      countableList.decoder,
      Decoders.doubleStringDecoder.map( _.seconds ),
      buildablesList.decoder.map( _.toList )
    )
  }

  implicit def recipeShow[N: Show]: Show[Recipe[N]] =
    Show.show {
      case Recipe( className, displayName, ingredients, products, duration, producers ) =>
        show"""  $displayName # $className
              |  Ingredients: 
              |    ${ingredients.map( _.show ).mkString( "\n    " )}
              |  Products:
              |    ${products.map( _.show ).toList.mkString( "\n    " )}
              |  Duration $duration
              |  Producers:
              |    ${producers.map( _.show ).mkString( "\n    " )}
              |""".stripMargin
    }

}
