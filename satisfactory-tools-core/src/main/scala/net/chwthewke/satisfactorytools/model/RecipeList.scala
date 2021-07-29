package net.chwthewke.satisfactorytools
package model

import cats.Eq
import cats.Show

case class RecipeList( recipes: Vector[Recipe] )

object RecipeList {
  implicit val recipeListShow: Show[RecipeList] =
    Show.show( _.recipes.map( _.displayName ).mkString( "\n" ) )

  implicit val recipeListEq: Eq[RecipeList] = Eq.by( _.recipes.map( _.className ).toSet )

}
