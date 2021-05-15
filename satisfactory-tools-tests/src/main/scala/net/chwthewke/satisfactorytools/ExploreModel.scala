package net.chwthewke.satisfactorytools

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.foldable._
import cats.syntax.show._

import data.Loader
import model.Model

object ExploreModel extends IOApp {
  override def run( args: List[String] ): IO[ExitCode] =
    Loader.io.loadModel
      .flatMap( printManufacturers )
      .as( ExitCode.Success )

  def printManufacturers( model: Model ): IO[Unit] = {
    val manufacturers =
      model.manufacturingRecipes.foldMap( recipe => Set( recipe.producedIn ) ).toVector.sortBy( _.displayName )

    IO.println( show"""MANUFACTURERS
                      |
                      |${manufacturers.mkString_( "\n\n" )}
                      |""".stripMargin )
  }
}
