package net.chwthewke.satisfactorytools
package model

import cats.Show

case class RecipeList( recipes: Vector[Recipe] )

object RecipeList {
  implicit val recipeListShow: Show[RecipeList] =
    Show.show( _.recipes.map( _.displayName ).mkString( "\n" ) )
}
