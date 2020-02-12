package net.chwthewke.satisfactorytools

import cats.Show
import cats.Traverse
import cats.data.NonEmptyList
import cats.derived.semi
import cats.instances.string._
import cats.syntax.show._
import com.github.ghik.silencer.silent

final case class Recipe[N](
    className: ClassName,
    displayName: String,
    ingredients: NonEmptyList[Countable[N]],
    product: NonEmptyList[Countable[N]],
    producers: NonEmptyList[ClassName]
)

object Recipe {

  @silent( "local type Apply0 is never used" )
  implicit val recipeTraverse: Traverse[Recipe] = {
    semi.traverse
  }

  implicit def recipeShow[N: Show]: Show[Recipe[N]] =
    Show.show {
      case Recipe( className, displayName, ingredients, product, _ ) =>
        show"""  $displayName # $className
              |  Ingredients: 
              |    ${ingredients.map( _.show ).toList.mkString( "\n    " )}
              |  Products:
              |    ${product.map( _.show ).toList.mkString( "\n    " )}
              |""".stripMargin
    }

}
