package net.chwthewke.satisfactorytools
package prod

import alleycats.std.iterable._
import cats.Show
import cats.data.ZipVector
import cats.instances.double._
import cats.instances.map._
import cats.instances.string._
import cats.instances.vector._
import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.semigroup._
import cats.syntax.show._
//
import model.Countable
import model.Item

final case class Factory( bill: Bill, blocks: Vector[FactoryBlock] ) {

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

  def ingredientTree( item: Item, destinations: Vector[( String, Double )] ): String = {
    def destinationLine( dest: String, amount: Double ): String = {
      val amountText = f"$amount%4.3f".reverse.padTo( 8, ' ' ).reverse
      show"  $amountText -> $dest"
    }

    show"""${item.displayName}
          |${destinations.map( (destinationLine _).tupled ).intercalate( "\n" )}
          |""".stripMargin
  }

  def ingredientsTree: String = {
    val internalDestinations: Map[Item, Vector[( String, Double )]] =
      blocks.foldMap( _.ingredients )

    val billDestinations: Map[Item, Vector[( String, Double )]] =
      bill.items.foldMap {
        case Countable( item, amount ) => Map( item -> Vector( "STORAGE" -> amount ) )
      }

    (internalDestinations |+| billDestinations)
      .map( (ingredientTree _).tupled )
      .to( Iterable )
      .intercalate( "\n" )
  }

}

object Factory {
  implicit val factoryShow: Show[Factory] = Show.show { factory =>
    show"""BLOCKS
          |
          |${factory.blocksTable}
          |
          |
          |INGREDIENTS
          |
          |${factory.ingredientsTree}
          |""".stripMargin
  }
}
