package net.chwthewke.satisfactorytools

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.foldable._

import data.Loader
import model.Item
import model.MachineType
import model.Model
import model.Recipe

object PrintConfigStub extends IOApp {

  override def run( args: List[String] ): IO[ExitCode] =
    Loader.io.loadModel
      .flatMap( model => IO.println( configStub( model ) ) )
      .as( ExitCode.Success )

  def recipeLine( recipe: Recipe[_, Item], w: Int ): String = {
    val recipeKey = "\"" + recipe.className.name + "\""
    s"""  ${recipeKey.padTo( w + 2, ' ' )}    # ${recipe.displayName}"""
  }

  def recipeLines( recipes: Vector[Recipe[_, Item]] ): String = {
    val w = recipes.map( _.className.name.length ).max

    recipes
      .sortBy( r => ( r.product.head.item.displayName, r.isAlternate, r.displayName ) )
      .map( recipeLine( _, w ) )
      .intercalate( "\n" )
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
    val eligibleRecipes =
      model.manufacturingRecipes.filter( _.producedIn.machineType == MachineType.Manufacturer )

    val eligibleItems =
      model.items.values
        .filter( item => eligibleRecipes.exists( recipe => recipe.product.exists( _.item == item ) ) )
        .toVector

    s"""
       |items = {
       |${itemLines( eligibleItems, "0.0" )}
       |}
       |
       |recipes = [
       |${recipeLines( eligibleRecipes )}
       |]
       |""".stripMargin
  }

}
