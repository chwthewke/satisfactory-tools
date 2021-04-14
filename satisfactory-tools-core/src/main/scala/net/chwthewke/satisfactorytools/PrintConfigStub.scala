package net.chwthewke.satisfactorytools

import cats.effect.IO
import cats.syntax.foldable._

import model.Item
import model.MachineType
import model.Model
import model.Recipe

object PrintConfigStub extends Program[Unit] {
  override def runProgram( model: Model, config: Unit ): IO[Unit] =
    IO.delay( println( configStub( model ) ) )

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
      model.manufacturingRecipes
        .filter( _.producers.exists( machine => machine.machineType == MachineType.Manufacturer ) )

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
