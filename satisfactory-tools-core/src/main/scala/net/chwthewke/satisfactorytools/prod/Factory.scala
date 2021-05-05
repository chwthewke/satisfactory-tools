package net.chwthewke.satisfactorytools
package prod

import alleycats.std.iterable._
import cats.Order.catsKernelOrderingForOrder
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
import model.Machine
import model.Recipe

final case class Factory(
    extraction: Vector[ClockedRecipe],
    manufacturing: Vector[Countable[Double, Recipe[Machine, Item]]],
    extraInputs: Vector[Countable[Double, Item]],
    extraOutputs: Vector[Countable[Double, Item]]
) {

  import FactoryTable.alignment
  import FactoryTable.headers

  def allRecipes: Vector[ClockedRecipe] = extraction ++ manufacturing.map( ClockedRecipe.roundUp )

  def tableColumns( clockedRecipe: ClockedRecipe ): Vector[String] = {
    import clockedRecipe._

    FactoryTable.columnsForBlock(
      recipe.item.displayName,
      productsPerMinute.head.amount,
      productsPerMinute.head.amount / machineCount,
      machine.displayName,
      machineCount,
      clockSpeedMillionth,
      power
    )
  }

  def blocksTable: String = {
    val tableRows = allRecipes.map( tableColumns )
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
        ("TOTAL POWER" +: Vector.fill( 11 )( "" )) ++ Vector( f"${allRecipes.foldMap( _.power )}%6.2f", " MW" )
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
    extraction
      .foldMap( cr => Map( ( cr.products.head.item.displayName, cr.productsPerMinute.head.amount ) ) )
      .toVector
      .sortBy { case ( p, x ) => ( -x, p ) }
      .map { case ( p, x ) => f"${p.padTo( 24, ' ' )} $x%.3f" }
      .intercalate( "\n" )

  def ingredientTree( item: Item, destinations: SortedSet[( Direction, String, Double )] ): String = {
    def destinationLine( direction: Direction, dest: String, amount: Double ): String = {
      val amountText = f"$amount%4.3f".reverse.padTo( 8, ' ' ).reverse
      show"  $amountText ${direction.arrow} $dest"
    }

    show"""${item.displayName}
          |${destinations.toVector.map( (destinationLine _).tupled ).intercalate( "\n" )}
          |""".stripMargin
  }

  def ingredientsTree( bill: Bill ): String = {

    def inputsOutputs( cr: ClockedRecipe ): SortedMap[Item, SortedSet[( Direction, String, Double )]] = {
      def line(
          direction: Direction,
          lineItem: Countable[Double, Item]
      ): SortedSet[( Direction, String, Double )] =
        SortedSet( ( direction, cr.recipe.item.displayName, lineItem.amount ) )

      val ingredients =
        cr.ingredientsPerMinute
          .foldMap( ci => SortedMap( ( ci.item, line( Direction.In, ci ) ) ) )
      val products =
        cr.productsPerMinute
          .foldMap( ci => SortedMap( ( ci.item, line( Direction.Out, ci ) ) ) )

      ingredients ++ products
    }

    val internalDestinations: SortedMap[Item, SortedSet[( Direction, String, Double )]] =
      allRecipes.foldMap( inputsOutputs )

    val billDestinations: SortedMap[Item, SortedSet[( Direction, String, Double )]] =
      bill.items.foldMap {
        case billItem @ Countable( item, _ ) =>
          SortedMap(
            item -> SortedSet[( Direction, String, Double )](
              ( Direction.In, "STORAGE", billItem.amount )
            )
          )
      }

    val extraOutputDestinations: SortedMap[Item, SortedSet[( Direction, String, Double )]] =
      extraOutputs
        .map {
          case Countable( item, amount ) =>
            (
              item,
              SortedSet[( Direction, String, Double )]( ( Direction.In, "EXTRA", amount ) )
            )
        }
        .to( SortedMap )

    (internalDestinations |+| billDestinations |+| extraOutputDestinations)
      .map( (ingredientTree _).tupled )
      .to( Iterable )
      .intercalate( "\n" )
  }

  def render( bill: Bill ): String =
    show"""BLOCKS
          |
          |$blocksTable
          |
          |
          |RAW RESOURCES
          |
          |$extractedResources
          |
          |INGREDIENTS
          |
          |${ingredientsTree( bill )}
          |""".stripMargin

}
