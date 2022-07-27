package net.chwthewke.satisfactorytools

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.foldable._

import data.Item
import loader.Loader
import model.Model
import model.Recipe

object PrintConfigStub extends IOApp {

  override def run( args: List[String] ): IO[ExitCode] =
    Loader.io
      .loadModel( DataVersionStorage.Update5 )
      .flatMap( model => IO.println( configStub( model ) ) )
      .as( ExitCode.Success )

  def recipeLine( recipe: Recipe, w: Int ): String = {
    val recipeKey = "\"" + recipe.className.name + "\""
    s"""  ${recipeKey.padTo( w + 2, ' ' )}    # ${recipe.displayName}"""
  }

  def recipeLines( recipes: Vector[Recipe] ): Vector[String] = {
    val w = recipes.map( _.className.name.length ).max

    recipes
      .sortBy( r => ( r.products.head.item.displayName, r.isAlternate, r.displayName ) )
      .map( recipeLine( _, w ) )
  }

  def itemLine( item: Item, w: Int, defaultValue: String ): String = {
    val itemKey = "\"" + item.className.name + "\":"
    s"""  ${itemKey.padTo( w + 4, ' ' )}    $defaultValue  # ${item.displayName}"""
  }

  def itemLines( items: Vector[Item], defaultValue: String ): String = {
    val w = items.map( _.className.name.length ).max
    items.sortBy( _.displayName ).map( itemLine( _, w, defaultValue ) ).intercalate( "\n" )
  }

  def configStub( model: Model ): String = {
    val eligibleItems =
      model.items.values
        .filter( item => model.manufacturingRecipes.exists( recipe => recipe.products.exists( _.item == item ) ) )
        .toVector

    s"""
       |items = {
       |${itemLines( eligibleItems, "0.0" )}
       |}
       |
       |recipes = [
       |${recipeLines( model.manufacturingRecipes ).intercalate( "\n" )}
       |]
       |
       |forbidden = [
       |${recipeLines( model.manufacturingRecipes ).map( line => s"// $line" ).intercalate( "\n" )}
       |]
       |""".stripMargin
  }

}
