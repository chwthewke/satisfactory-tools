package net.chwthewke.satisfactorytools

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.show._

import data.GameData
import loader.Loader

object ExploreGameData extends IOApp {
  override def run( args: List[String] ): IO[ExitCode] =
    Loader.io.loadGameData
      .flatMap( printRecipes )
      .as( ExitCode.Success )

  def printRecipes( data: GameData ): IO[Unit] =
    IO.println(
      show"""RECIPES
      ${data.recipes
              .map( recipe => show"${recipe.displayName} [${recipe.className}] in ${recipe.producedIn.headOption}" )
              .mkString( "\n" )}
            |""".stripMargin
    )

}
