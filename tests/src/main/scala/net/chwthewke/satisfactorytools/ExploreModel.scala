package net.chwthewke.satisfactorytools

import cats.Order.catsKernelOrderingForOrder
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.show._

import loader.Loader
import model.Model
import web.view.RecipesView

object ExploreModel extends IOApp {
  override def run( args: List[String] ): IO[ExitCode] =
    Loader.io
      .loadModel( DataVersionStorage.Update7 )
      .flatMap( printCatalysts )
      .as( ExitCode.Success )

  def printManufacturers( model: Model ): IO[Unit] = {
    val manufacturers =
      model.manufacturingRecipes.foldMap( recipe => Set( recipe.producedIn ) ).toVector.sortBy( _.displayName )

    IO.println( show"""MANUFACTURERS
                      |
                      |${manufacturers.mkString_( "\n\n" )}
                      |""".stripMargin )
  }

  def printExtractionRecipes( model: Model ): IO[Unit] =
    IO.println(
      model.extractionRecipes
        .map( _._3.className )
        .sorted
        .mkString_( s"EXTS (${model.extractionRecipes.size})\n", "\n", "" )
    )

  def printCatalysts( model: Model ): IO[Unit] =
    IO.println(
      model.manufacturingRecipes
        .foldMap( recipe =>
          recipe.ingredients
            .map( _.item )
            .intersect( recipe.products.toList.map( _.item ) )
            .tupleRight( Vector( recipe ) )
            .toMap
        )
        .map {
          case ( item, recipes ) =>
            s"${item.displayName} in ${recipes.map( r => s"${r.displayName} ${RecipesView.describeRecipe( r )}" ).mkString( ", " )}"
        }
        .mkString( "CATALYSTS\n  ", "\n  ", "" )
    )
}
