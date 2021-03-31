package net.chwthewke.satisfactorytools

import cats.Monoid
import cats.effect.Blocker
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.foldable._
import io.circe.Decoder
import org.scalameta.ascii.graph.Graph
import org.scalameta.ascii.layout._
import pureconfig.ConfigSource
import pureconfig.module.catseffect.syntax._
//
import load.Loader
import model.Model
import model.ProtoModel
import prod.ProductionConfig
import prod.RecipeMatrix

object ExploreRecipeGraph extends IOApp {

  override def run( args: List[String] ): IO[ExitCode] =
    for {
      ( data, config ) <- Blocker[IO]
                           .use( blocker => ( loadData[ProtoModel]( blocker ), loadConfig( blocker ) ).tupled )
      model <- data.toModel.toEither.leftMap( m => new RuntimeException( m.intercalate( ", " ) ) ).liftTo[IO]
      _     <- IO.delay( println( asciiGraph( config, model ) ) )
    } yield ExitCode.Success

  def asciiGraph( config: ProductionConfig, model: Model ): String = {
    val mat = RecipeMatrix.init( config, model )

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

  def loadData[A: Decoder: Monoid]( blocker: Blocker ): IO[A] =
    Loader.io.loadResource[A]( blocker )

  def loadConfig( blocker: Blocker ): IO[ProductionConfig] =
    ConfigSource.default.loadF[IO, ProductionConfig]( blocker )

}
