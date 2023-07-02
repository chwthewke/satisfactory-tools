package net.chwthewke.satisfactorytools

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.foldable._
import cats.syntax.show._

import data.GameData
import loader.Loader

object ExploreGameData extends IOApp {
  override def run( args: List[String] ): IO[ExitCode] =
    Loader.io
      .loadGameData( DataVersionStorage.Update7 )
      .flatMap(
        data =>
          IO.println(
            data.manufacturers.keys.mkString( "MANUFS\n", "\n", "\n" )
          ) *>
            IO.print(
              data.recipes.foldMap( _.producedIn ).distinct.mkString( "PRODS\n", "\n", "\n" )
            )
      )
      .as( ExitCode.Success )

  def printRecipes( data: GameData ): IO[Unit] =
    IO.println(
      show"""RECIPES
            |${data.recipes
              .map(
                recipe =>
                  show"""${recipe.displayName} [${recipe.className}] in ${recipe.producedIn.headOption}
                        |  ${recipe.ingredients
                          .map( _.item.name )
                          .mkString( ", " )} -> ${recipe.products.map( _.item.name ).mkString_( ", " )}
                        |""".stripMargin
              )
              .mkString}
            |""".stripMargin
    )

  def printManufacturers( data: GameData ) =
    IO.println(
      data.manufacturers.values
        .map( m => show"${m.displayName} [${m.className}]" )
        .mkString( "MANUFACTURERS\n", "\n", "" )
    )

  def printItemIcons( data: GameData ): IO[Unit] =
    IO.println(
      show"""ITEM ICONS
            |${data.items.values
              .map( _._1 )
              .toVector
              .sortBy( _.displayName )
              .map( item => show"${item.displayName} => ${item.smallIcon}" )
              .mkString( "\n" )}
            |""".stripMargin
    )

  def printSchematicTypes( data: GameData ): IO[Unit] =
    IO.println(
      show"""SCHEMATIC TYPES
            |${data.schematics.map( _.`type` ).distinct.mkString_( "\n" )}
            |""".stripMargin
    )

}
