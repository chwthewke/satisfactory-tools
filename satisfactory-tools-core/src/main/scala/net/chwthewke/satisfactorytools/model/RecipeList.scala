package net.chwthewke.satisfactorytools
package model

import cats.Show

import data.Item

case class RecipeList( recipes: Vector[Recipe[Machine, Item]] )

object RecipeList {
  implicit val recipeListShow: Show[RecipeList] =
    Show.show( _.recipes.map( _.displayName ).mkString( "\n" ) )
}
