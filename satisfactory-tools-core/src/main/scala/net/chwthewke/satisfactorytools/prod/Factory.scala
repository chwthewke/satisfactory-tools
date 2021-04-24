package net.chwthewke.satisfactorytools
package prod

import alleycats.std.iterable._
import cats.Order.catsKernelOrderingForOrder
import cats.Show
import cats.data.ZipVector
import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.semigroup._
import cats.syntax.show._
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

import model.Bill
import model.Countable
import model.Item

final case class Factory( bill: Bill, blocks: Vector[FactoryBlock], extraInputs: Vector[Countable[Item, Double]] ) {

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

  def extractedResources: String =
    blocks
      .filter( _.recipe.item.isExtraction )
      .foldMap {
        case FactoryBlock( Countable( recipe, amount ), _ ) =>
          val product = recipe.productsPerMinute.head
          Map( ( product.item.displayName, product.amount * amount ) )
      }
      .toVector
      .sortBy { case ( p, x ) => ( -x, p ) }
      .map { case ( p, x ) => f"${p.padTo( 24, ' ' )} $x%.3f" }
      .intercalate( "\n" )

  def ingredientTree( item: Item, destinations: SortedSet[( FactoryBlock.Direction, String, Double )] ): String = {
    def destinationLine( direction: FactoryBlock.Direction, dest: String, amount: Double ): String = {
      val amountText = f"$amount%4.3f".reverse.padTo( 8, ' ' ).reverse
      show"  $amountText ${direction.arrow} $dest"
    }

    show"""${item.displayName}
          |${destinations.toVector.map( (destinationLine _).tupled ).intercalate( "\n" )}
          |""".stripMargin
  }

  def ingredientsTree: String = {
    val internalDestinations: SortedMap[Item, SortedSet[( FactoryBlock.Direction, String, Double )]] =
      blocks.foldMap( _.inputsOutputs )

    val billDestinations: SortedMap[Item, SortedSet[( FactoryBlock.Direction, String, Double )]] =
      bill.items.foldMap {
        case billItem @ Countable( item, _ ) =>
          SortedMap(
            item -> SortedSet[( FactoryBlock.Direction, String, Double )](
              ( FactoryBlock.Direction.In, "STORAGE", billItem.amount )
            )
          )
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
          |RAW RESOURCES
          |
          |${factory.extractedResources}
          |
          |INGREDIENTS
          |
          |${factory.ingredientsTree}
          |""".stripMargin
  }
}
