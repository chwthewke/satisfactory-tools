package net.chwthewke.satisfactorytools

import cats.effect.IO
import cats.instances.string._
import cats.instances.vector._
import cats.syntax.foldable._
//
import model.Item
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
      .sortBy( r => ( r.product.head.item.displayName, r.className.name ) )
      .map( recipeLine( _, w ) )
      .intercalate( "\n" )
  }

  def itemLine( item: Item, w: Int ): String = {
    val itemKey = "\"" + item.className.name + "\":"
    s"""  ${itemKey.padTo( w + 4, ' ' )}    0.0  # ${item.displayName}"""
  }

  def itemLines( items: Vector[Item] ): String = {
    val w = items.map( _.className.name.length ).max
    items.sortBy( _.displayName ).map( itemLine( _, w ) ).intercalate( "\n" )
  }

  def configStub( model: Model ): String =
    s"""
       |items = {
       |${itemLines( model.items.values.toVector )}
       |}
       |
       |recipes = [
       |${recipeLines( model.recipes )}
       |]
       |""".stripMargin

}
