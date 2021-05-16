package net.chwthewke.satisfactorytools
package text

import alleycats.std.iterable._
import cats.Order.catsKernelOrderingForOrder
import cats.data.ZipVector
import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.semigroup._
import cats.syntax.show._
import mouse.boolean._
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

import data.Countable
import data.Item
import model.Bill
import prod.ClockedRecipe
import prod.Direction
import prod.Factory

object FactoryTable {
  import Alignment.AlignLeft
  import Alignment.AlignRight

  val sep = " | "

  def columnsForBlock(
      recipeName: String,
      totalAmount: Double,
      amountPerUnit: Double,
      machineName: String,
      machineCount: Int,
      clockSpeed10: Int,
      power: Double
  ): Vector[String] =
    Vector(
      f"$totalAmount%4.3f",
      sep,
      recipeName.stripPrefix( "Alternate: " ),
      " ",
      recipeName.startsWith( "Alternate: " ).fold( "ALT", "" ),
      sep,
      f"$machineCount%3d",
      " ",
      machineName,
      sep,
      f"$amountPerUnit%3.3f / unit @ ${clockSpeed10 / 10000}%3d.${clockSpeed10 % 10000}%04d %%",
      sep,
      f"$power%4.2f",
      f" MW"
    )

  val headers =
    Vector(
      ( 5, "Recipe" ),
      ( 1, sep ),
      ( 3, "Machines" ),
      ( 1, sep ),
      ( 1, "Rate" ),
      ( 1, sep ),
      ( 2, "Power" )
    )

  val alignment =
    Vector(
      AlignRight,
      AlignLeft,
      AlignLeft,
      AlignLeft,
      AlignRight,
      //
      AlignLeft, // sep
      //
      AlignRight,
      AlignLeft,
      AlignLeft,
      //
      AlignLeft, // sep
      //
      AlignRight,
      //
      AlignLeft, // sep
      //
      AlignRight,
      AlignLeft
    )

  def render( bill: Bill, factory: Factory ): String =
    show"""BLOCKS
          |
          |${blocksTable( factory )}
          |
          |
          |RAW RESOURCES
          |
          |${extractedResources( factory )}
          |
          |INGREDIENTS
          |
          |${ingredientsTree( bill, factory )}
          |""".stripMargin

  private def tableColumns( clockedRecipe: ClockedRecipe ): Vector[String] = {
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

  private def blocksTable( factory: Factory ): String = {
    val tableRows = factory.allRecipes.map( tableColumns )
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
        ("TOTAL POWER" +: Vector.fill( 11 )( "" )) ++ Vector( f"${factory.allRecipes.foldMap( _.power )}%6.2f", " MW" )
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

  private def extractedResources( factory: Factory ): String =
    factory.extraction
      .foldMap( cr => Map( ( cr.products.head.item.displayName, cr.productsPerMinute.head.amount ) ) )
      .toVector
      .sortBy { case ( p, x ) => ( -x, p ) }
      .map { case ( p, x ) => f"${p.padTo( 24, ' ' )} $x%.3f" }
      .intercalate( "\n" )

  private def ingredientTree( item: Item, destinations: SortedSet[( Direction, String, Double )] ): String = {
    def destinationLine( direction: Direction, dest: String, amount: Double ): String = {
      val amountText = f"$amount%4.3f".reverse.padTo( 8, ' ' ).reverse
      show"  $amountText ${direction.arrow} $dest"
    }

    show"""${item.displayName}
          |${destinations.toVector.map( (destinationLine _).tupled ).intercalate( "\n" )}
          |""".stripMargin
  }

  def ingredientsTree( bill: Bill, factory: Factory ): String = {

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
      factory.allRecipes.foldMap( inputsOutputs )

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
      factory.extraOutputs
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

}
