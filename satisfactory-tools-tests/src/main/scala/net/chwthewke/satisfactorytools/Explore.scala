package net.chwthewke.satisfactorytools

import cats.effect.Blocker
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.instances.vector._
import cats.syntax.functor._
import cats.syntax.functorFilter._
//import spire.math.Rational
//
//import alg.Matrix
import load.Loader
import model._

object Explore extends IOApp {
  override def run( args: List[String] ): IO[ExitCode] = {
    Blocker[IO]
      .use( blocker => Loader.io( blocker ).load() )
//      .map( filterRecipes )
      .flatMap( m => IO( println( m ) ) )
      .as( ExitCode.Success )
  }

  def filterRecipes( model: Model ): Vector[Recipe[ClassName]] =
    model.recipes.mapFilter(
      recipe =>
        if (recipe.producers.exists( Manufacturer.builders ) && !recipe.displayName.toLowerCase
              .startsWith( "alternate" ))
          Some( recipe )
        else if (recipe.producers.contains( Extractor.extractorClass ))
          Some( recipe.copy( ingredients = Nil ) )
        else
          None
    )

  def index( model: Model, recipes: Vector[Recipe[ClassName]] ): Vector[ClassName] =
    recipes.flatMap( r => r.ingredients ++ r.product.toList ).map( _.item ).filter( model.items.keySet )

//  def matrixOf( recipe: Recipe[ClassName], index: Vector[ClassName] ): Matrix[Rational] = {
//    val withIndices = recipe.fproduct( index.indexOf )
//    val denom = recipe.product.head.amount
//
//    Matrix.eval( index.size, index.size, (i, j) =>
//
//    )
//  }

}
