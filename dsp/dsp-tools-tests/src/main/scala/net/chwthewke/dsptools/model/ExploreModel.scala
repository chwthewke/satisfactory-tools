package net.chwthewke.dsptools
package model

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.foldable._
import cats.syntax.show._

import loader.Loader

object ExploreModel extends IOApp {
  override def run( args: List[String] ): IO[ExitCode] =
    Loader.io.loadModel
      .map(
        _.recipes
//        model => Vector( "ITEMS" ) ++ productiveItems( model ) ++ Vector( "", "RECIPES" ) ++ productiveRecipes( model )
      )
      .flatMap(
        _.traverse_( x => IO.println( x.show ) )
      )
      .as( ExitCode.Success )

  def inputOnlyItems( recipes: Vector[Recipe] ): Vector[Item] = {
    val ( allIngredients, allProducts ) =
      recipes.foldMap(
        recipe =>
          (
            recipe.ingredients.map( _.item ).toSet,
            recipe.products.map( _.item ).toVector.toSet
          )
      )

    allIngredients.diff( allProducts ).toVector
  }

  def productiveItems( model: Model ): Vector[String] =
    model.items.map( item => show"${item.displayName} PRODUCTIVE: ${if (item.productive) "YES" else "NO"}" )

  def productiveRecipes( model: Model ): Vector[String] =
    model.recipes.map( recipe => show"${recipe.displayName} PRODUCTIVE: ${if (recipe.productive) "YES" else "NO"}" )

  def dumpableRecipes( model: Model ): Vector[String] =
    model.recipes.map(
      recipe => show"${recipe.id}, // ${recipe.displayName}"
    )

  private def w( itemId: Int ) =
    if (itemId < 1007 || itemId == 1120) 1
    else if (itemId < 1100) 2
    else if (itemId == 1121) 10
    else 4

  def dumpableItems( model: Model ): Vector[String] =
    model.items.map { item =>
      show"${item.id} -> ${w( item.id )}, // ${item.displayName}"
    }

}
