package net.chwthewke.satisfactorytools

import cats.effect.IO
import cats.instances.string._
import cats.instances.vector._
import cats.syntax.foldable._
//
import model.Model
import prod.ProductionConfig
import prod.RecipeMatrix

object DumpMatrixCsv extends Program[ProductionConfig] {

  override def runProgram( model: Model, config: ProductionConfig ): IO[Unit] = {

    val matrix = RecipeMatrix.init( config, model )

    val output = (
      ("" +: matrix.columnLabels.map( _.displayName )).intercalate( "," ) +:
        matrix.rowLabels.zipWithIndex.map {
          case ( it, ix ) =>
            (
              it.displayName +: (0 until matrix.matrix.cols)
                .map( matrix.matrix( ix, _ ) )
                .map( v => f"$v%4.3f" )
                .toVector
            ).intercalate( "," )
        }
    ).intercalate( "\n" )

    IO.delay( println( output ) )
  }

}
