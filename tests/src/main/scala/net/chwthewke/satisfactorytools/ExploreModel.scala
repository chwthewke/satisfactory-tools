package net.chwthewke.satisfactorytools

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all._
import scala.collection.immutable.SortedMap

import loader.Loader
import model.Model

object ExploreModel extends IOApp {
  override def run( args: List[String] ): IO[ExitCode] =
    Loader.io
      .loadModel( DataVersionStorage.Release1_0 )
      .flatTap( printRecipesByTier )
      .as( ExitCode.Success )

  def printExtractionRecipes( model: Model ): IO[Unit] =
    IO.println(
      model.extractionRecipes.mkString_( "\n" )
    )

  def printRecipesByTier( model: Model ): IO[Unit] =
    IO.println(
      model.manufacturingRecipes
        .groupBy( _.category.tierOpt )
        .to( SortedMap )
        .map {
          case ( to, recs ) =>
            recs
              .map( r => s"${r.displayName.padTo( 64, ' ' )}${Option.when( r.isAlternate )( "ALT" ).orEmpty}" )
              .mkString(
                show"""${to.fold( "Unknown tier" )( t => s"Tier $t" )}\n  """,
                "\n  ",
                ""
              )
        }
        .mkString( "\n" )
    )
}
