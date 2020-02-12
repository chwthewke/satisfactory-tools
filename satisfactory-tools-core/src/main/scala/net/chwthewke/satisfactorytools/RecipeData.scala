package net.chwthewke.satisfactorytools

import cats.Monoid
import cats.Show
import cats.derived.semi
import cats.instances.map._
import cats.instances.string._
import cats.instances.vector._
import cats.syntax.show._

case class RecipeData[N]( itemDescriptions: Map[ClassName, String], recipes: Vector[Recipe[N]] )

object RecipeData {
  implicit def recipeDataMonoid[N]: Monoid[RecipeData[N]] =
    semi.monoid

  def empty[N]: RecipeData[N] = recipeDataMonoid[N].empty

  implicit def recipeDataShow[N: Show]: Show[RecipeData[N]] = Show.show {
    case RecipeData( itemDescriptions, recipes ) =>
      show"""Items:
            |  ${itemDescriptions.map { case ( cn, dn ) => show"$dn # $cn" }.mkString( "\n  " )}
            |
            |Recipes:
            |${recipes.map( _.show ).mkString( "\n" )}
            |""".stripMargin
  }
}
