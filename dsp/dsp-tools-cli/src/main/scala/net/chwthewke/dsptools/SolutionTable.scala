package net.chwthewke.dsptools

import cats.Id
import cats.Order.catsKernelOrderingForOrder
import cats.data.ZipVector
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.reducible._
import cats.syntax.show._
import cats.syntax.traverse._
import enumeratum.Enum
import enumeratum.EnumEntry

import net.chwthewke.factory.data.Countable
import net.chwthewke.factory.prod.Direction
import model.Item
import model.Model
import model.Recipe
import prod.solver.ModifiedRecipe
import prod.solver.Solution

object SolutionTable {
  sealed abstract class Alignment( val op: String => String ) extends EnumEntry with Product {
    def pad( text: String, width: Int ): String = op( op( text ).padTo( width, ' ' ) )
  }

  object Alignment extends Enum[Alignment] {
    final case object AlignLeft  extends Alignment( identity[String] )
    final case object AlignRight extends Alignment( _.reverse )

    override val values: IndexedSeq[Alignment] = findValues
  }

  import Alignment.AlignLeft
  import Alignment.AlignRight
  import Direction.Provide
  import Direction.Receive

  val Tolerance: Double = 1e-4

  val sep = " | "

  def columnsForRecipe( recipe: Countable[Double, ModifiedRecipe] ): Vector[String] =
    Vector(
      fmtCeilD3( recipe.amount ),
      sep,
      s"(${fmtAmountD3( recipe.amount )})",
      sep,
      recipe.item.recipeType.show,
      sep,
      recipe.item.displayName,
      sep,
      fmtAmountD5( recipe.flatMap( _.productsPerMinute.head ).amount ),
      " ",
      recipe.item.products.head.item.displayName
    )

  val headers =
    Vector(
      ( 3, "Amount" ),
      ( 1, sep ),
      ( 1, "Type" ),
      ( 1, sep ),
      ( 1, "Recipe" ),
      ( 1, sep ),
      ( 3, "Product" )
    )

  val alignment =
    Vector(
      AlignRight,
      AlignLeft,
      AlignRight,
      AlignLeft,
      AlignLeft,
      AlignLeft,
      AlignLeft,
      AlignLeft,
      AlignRight,
      AlignLeft,
      AlignLeft
    )

  private def fmtAmountD3( amount: Double ): String = f"$amount%3.3f"
  private def fmtAmountD5( amount: Double ): String = f"$amount%5.3f"
  private def fmtCeilD3( amount: Double ): String   = f"${amount.ceil.toInt}%3d"

  def reachable( recipe: Recipe, fromItems: Set[Item] ): Boolean =
    recipe.ingredients.forall { case Countable( item, _ ) => fromItems( item ) }

  def sortRecipes( initialItems: Set[Item], recipes: Vector[Recipe] ): Vector[Recipe] = {
    //val initialItems = solution.inputs.map( _.item ).toSet
    (
      Vector.empty[Recipe],
      initialItems,
      recipes.partition( reachable( _, initialItems ) )
    ).tailRecM[Id, Vector[Recipe]] {
      case ( acc, seen, ( available, rest ) ) =>
        if (available.isEmpty)
          Right( acc ++ rest )
        else {
          val next     = acc ++ available
          val nextSeen = seen.union( available.foldMap( _.products.map( _.item ).toList.to( Set ) ) )

          Left( ( next, nextSeen, rest.partition( reachable( _, nextSeen ) ) ) )
        }
    }
  }

  def itemRanks( model: Model, inputs: Set[Item] ): Vector[( Item, Int )] = {
    val sortedRecipes = sortRecipes( inputs, model.recipes )
    model.items
      .fproduct(
        item =>
          Some( sortedRecipes.indexWhere( _.products.exists( _.item == item ) ) )
            .filter( _ >= 0 )
            .getOrElse( if (inputs( item )) -1 else Int.MaxValue )
      )
      .sortBy( _._2 )
  }

  def sortSolution( model: Model, solution: Solution ): Solution = {
    val initialItems = solution.inputs.map( _.item ).toSet
    val itemOrder    = itemRanks( model, initialItems )

    val sortedRecipes =
      solution.recipes
        .filter( _.amount > Tolerance )
        .sortBy( recipe => recipe.item.products.map( item => itemOrder.indexWhere( _._1 == item.item ) ).maximum )

    val sortedFlows =
      solution.flows
        .map {
          case ( item, flows ) =>
            ( item, flows.filter( _._3.abs > Tolerance ).sorted )
        }
        .filter( _._2.nonEmpty )
        .sortBy( _._1.displayName )

    Solution( sortedRecipes, solution.inputs.filter( _.amount.abs > Tolerance ), sortedFlows )
  }

  def blocksTable( recipes: Vector[Countable[Double, ModifiedRecipe]] ): String = {
    val tableRows = recipes.map( columnsForRecipe )
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

    val headersLine: String =
      headers
        .foldLeft( ( "", columnWidths ) ) {
          case ( ( acc, widths ), ( nCol, text ) ) =>
            ( acc + text.padTo( widths.take( nCol ).map( _._1 ).sum, ' ' ), widths.drop( nCol ) )
        }
        ._1

    val sepLine: String = "-" * columnWidths.map( _._1 ).sum

    (headersLine +: sepLine +: rowLines).intercalate( "\n" )

  }

  def inputs( solution: Solution ): String =
    solution.inputs
      .filter( _.amount.abs > Tolerance )
      .map( it => s"${fmtAmountD5( it.amount )} ${it.item.displayName}" )
      .intercalate( "\n" )

  def inputOutput( solution: Solution ): Map[Item, Map[( ModifiedRecipe, Direction ), Double]] =
    solution.recipes.filter( _.amount > Tolerance ).foldMap { recipe =>
      recipe.flatTraverse( _.itemsPerMinute ).foldMap {
        case Countable( item, amount ) =>
          Map( item -> Map( ( recipe.item, if (amount < 0) Provide else Receive ) -> amount ) )
      }
    }

  def showInputOutputs(
      item: Item,
      input: Option[Double],
      request: Option[Double],
      flows: Vector[( Direction, ModifiedRecipe, Double )]
  ): String = {
    def fmtFlow( desc: String, dir: Direction, amount: Double ): String =
      s"  ${AlignRight.pad( fmtAmountD5( amount.abs ), 10 )} ${dir.arrow} $desc"

    def destLineRecipe( recipe: ModifiedRecipe, direction: Direction, amount: Double ): String =
      fmtFlow( recipe.displayName, direction, amount )

    val destLineInput: Vector[String] =
      input.map( fmtFlow( "INPUT", Receive, _ ) ).toVector

    val destLinesInternal: Vector[String] =
      flows
        .foldMap { case ( direction, recipe, amount ) => Vector( destLineRecipe( recipe, direction, amount ) ) }

    val destLinesReq: Vector[String] = {
      val inOut  = flows.map( _._3 ) ++ input
      val output = inOut.combineAll.abs
      if (output < Tolerance)
        Vector.empty
      else {
        val ( requested, byProduct ) = request.fold( ( 0d, output ) )( r => ( r, output - r ) )
        Vector( ( "REQUEST", requested ), ( "BY-PRODUCT", byProduct ) )
          .filter( _._2 > Tolerance )
          .map { case ( desc, amt ) => fmtFlow( desc, Provide, amt ) }
      }

    }

    val lines: Vector[String] =
      destLineInput ++ destLinesInternal ++ destLinesReq

    s"""${item.displayName}
       |${lines.intercalate( "\n" )}
       |""".stripMargin
  }

  def showInputOutputs( requested: Vector[Countable[Double, Item]], solution: Solution ): String = {
    val extInputs: Map[Item, Double] =
      solution.inputs
        .filter( _.amount.abs > Tolerance )
        .foldMap { case Countable( item, amount ) => Map( item -> amount ) }

    solution.flows
      .map {
        case ( item, flows ) =>
          showInputOutputs(
            item,
            extInputs.get( item ),
            requested.find( _.item.id == item.id ).map( _.amount ),
            flows
          )
      }
      .intercalate( "\n\n" )
  }

  def apply( model: Model, requested: Vector[Countable[Double, Item]], solution: Solution ): String = {
    val preparedSolution = sortSolution( model, solution )

    s"""FACTORY
       |${blocksTable( preparedSolution.recipes )}
       |
       |INPUTS
       |
       |${inputs( preparedSolution )}
       |
       |DESTINATIONS
       |
       |${showInputOutputs( requested, preparedSolution )}
       |""".stripMargin
  }

}
