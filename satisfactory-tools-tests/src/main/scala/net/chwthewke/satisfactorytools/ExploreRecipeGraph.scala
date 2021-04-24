package net.chwthewke.satisfactorytools

import buildinfo.Satisfactorytools
import cats.effect.ExitCode
import cats.effect.IO
import cats.syntax.apply._
import cats.syntax.flatMap._
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import org.scalameta.ascii.graph.Graph
import org.scalameta.ascii.layout._

import data.Loader
import data.ProductionConfig
import model.Model

object ExploreRecipeGraph
    extends CommandIOApp(
      "test-ascii-graph",
      "Print the selected recipes as an ascii graph",
      version = Satisfactorytools.shortVersion
    ) {

  override def main: Opts[IO[ExitCode]] =
    Program.configOpt.map(
      cfg =>
        Loader.io.use(
          loader =>
            ( loader.loadModel, loader.loadProductionConfig( cfg ) )
              .mapN( runProgram )
              .flatten
              .as( ExitCode.Success )
        )
    )

  def runProgram( model: Model, config: ProductionConfig ): IO[Unit] =
    IO.delay( println( asciiGraph( config, model ) ) )

  def asciiGraph( config: ProductionConfig, model: Model ): String = {
    val mat = MkRecipeMatrix( config, model )

    val activeRecipes = mat.columnLabels

    val vertices = activeRecipes.map( _.displayName ).toSet

    val edges =
      ( activeRecipes, activeRecipes ).tupled
        .filter {
          case ( r, s ) =>
            r != s && r.product.exists( p => s.ingredients.exists( i => p.item == i.item ) )
        }
        .map {
          case ( r, s ) => ( r.displayName, s.displayName )
        }
        .toList

    GraphLayout.renderGraph( Graph( vertices, edges ) )
  }

}
