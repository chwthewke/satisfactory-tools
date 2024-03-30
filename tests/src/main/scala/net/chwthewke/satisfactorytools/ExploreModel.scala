package net.chwthewke.satisfactorytools

import cats.Order.catsKernelOrderingForOrder
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.foldable._
import cats.syntax.show._

import loader.Loader
import model.Model

object ExploreModel extends IOApp {
  override def run( args: List[String] ): IO[ExitCode] =
    Loader.io
      .loadModel( DataVersionStorage.Update7 )
      .flatMap( model =>
        IO.println(
          model.extractionRecipes
            .map( _._3.className )
            .sorted
            .mkString_( s"EXTS (${model.extractionRecipes.size})\n", "\n", "" )
        )
      )
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
