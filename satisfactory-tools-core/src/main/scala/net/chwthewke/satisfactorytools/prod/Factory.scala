package net.chwthewke.satisfactorytools
package prod

import cats.Show
import cats.data.ZipVector
import cats.instances.double._
import cats.instances.string._
import cats.instances.vector._
import cats.syntax.apply._
import cats.syntax.foldable._

final case class Factory( blocks: Vector[FactoryBlock] ) extends AnyVal {

  import FactoryTable.alignment
  import FactoryTable.headers

  def blocksTable: String = {
    val tableRows = blocks.map( _.tableColumns )
    val columnWidths: Vector[( Int, Alignment )] =
      tableRows
        .foldLeft( ZipVector( Vector.fill( alignment.size )( 0 ) ) )(
          ( acc, row ) => ( acc, ZipVector( row ) ).mapN( ( sz, col ) => sz max col.length )
        )
        .value
        .zip( alignment )

    def formatLine( cols: Vector[String] ): String =
      cols.zip( columnWidths ).foldMap { case ( text, ( width, align ) ) => align.pad( text, width ) }

    val rowLines: Vector[String] = tableRows.map( formatLine )

    val powerLine: String =
      formatLine(
        ("TOTAL POWER" +: Vector.fill( 11 )( "" )) ++ Vector( f"${blocks.foldMap( _.power )}%6.2f", " MW" )
      )

    val headersLine: String =
      headers
        .foldLeft( ( "", columnWidths ) ) {
          case ( ( acc, widths ), ( nCol, text ) ) =>
            ( acc + text.padTo( widths.take( nCol ).map( _._1 ).sum, ' ' ), widths.drop( nCol ) )
        }
        ._1

    val sepLine: String = "-" * columnWidths.map( _._1 ).sum

    (headersLine +: sepLine +: (rowLines :+ "" :+ powerLine)).intercalate( "\n" )

  }

}

object Factory {
  implicit val factoryShow: Show[Factory] = Show.show( _.blocksTable )
}
