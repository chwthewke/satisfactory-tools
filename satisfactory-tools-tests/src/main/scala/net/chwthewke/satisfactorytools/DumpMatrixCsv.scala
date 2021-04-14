package net.chwthewke.satisfactorytools

import cats.effect.IO
import cats.syntax.foldable._

import data.ProductionConfig
import model.Model

object DumpMatrixCsv extends Program[ProductionConfig]( "dump-matrix-csv", "Dump recipe matrix as csv" ) {

  override def runProgram( model: Model, config: ProductionConfig ): IO[Unit] = {

    val matrix = MkRecipeMatrix( config, model )

    val output = (
      ("" +: matrix.columnLabels.map( _.displayName )).intercalate( "," ) +:
        matrix.rowLabels.zipWithIndex.map {
          case ( it, ix ) =>
            (
              it.displayName +: (0L until matrix.matrix.countColumns)
                .map( matrix.matrix.get( ix.toLong, _ ) )
                .map( v => f"$v%4.3f" )
                .toVector
            ).intercalate( "," )
        }
    ).intercalate( "\n" )

    IO.delay( println( output ) )
  }

}
