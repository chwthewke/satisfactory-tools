package net.chwthewke.satisfactorytools
package web.view

import cats.Order.catsKernelOrderingForOrder
import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.option._
import cats.syntax.traverse._
import scala.collection.immutable.SortedMap
import scalatags.Text

import data.Countable
import data.Item
import model.Model
import prod.Factory
import web.protocol.Forms
import web.state.PageState

object CompareView {
  import Text.all._
  import Text.tags2.details
  import Text.tags2.summary

  val Tolerance: Double = 0.0005d

  def apply( model: Model, before: Option[PageState], after: Option[PageState] ): Tag =
    html(
      head( title := "Satisfactory Planner Comparator" ),
      body(
        content( model, before, after )
      )
    )

  def inputState( model: Model, eltId: String, inputName: String, state: Option[PageState] ): Tag =
    textarea(
      id := eltId,
      name := inputName,
      cols := 100,
      rows := 15,
      stateBase64( model, state )
    )

  def content( model: Model, before: Option[PageState], after: Option[PageState] ): Tag =
    div(
      form(
        action := "/compare",
        method := "POST",
        enctype := "application/x-www-form-urlencoded",
        div(
          display.flex,
          attr( "flex-flow" ) := "row",
          div(
            flex := "auto",
            div( label( `for` := "compare_input_before", "Before" ) ),
            div( inputState( model, "compare_input_before", Forms.compareBefore, before ) )
          ),
          div(
            flex := "auto",
            div( label( `for` := "compare_input_after", "After" ) ),
            div( inputState( model, "compare_input_after", Forms.compareAfter, after ) )
          )
        ),
        input( `type` := "submit", value := "Compare" )
      ),
      diffView( model, before, after )
    )

  def stateBase64( model: Model, state: Option[PageState] ): String =
    state.flatMap( PageState.toBase64( model, _ ).toOption ).orEmpty

  def diffView( model: Model, before: Option[PageState], after: Option[PageState] ): Tag =
    ( before.flatMap( _.factory ).flatMap( _.toOption ), after.flatMap( _.factory ).flatMap( _.toOption ) )
      .mapN(
        ( b, a ) =>
          div(
            inputOutputsDiff( b, a ),
            recipeDiff( model, b, a )
          )
      )
      .getOrElse( div( "Missing inputs or solution(s)" ) ) // TODO can be better

  ////////////////////////////
  // Inputs/Outputs

  def inputOutputs( factory: Factory ): ( SortedMap[Item, Double], SortedMap[Item, Double] ) = {
    val ( in, out ) =
      factory.manufacturing
        .foldMap(
          cr =>
            cr.flatTraverse( rec => rec.itemsPerMinute )
              .map { case Countable( item, amount ) => ( item, amount ) }
              .to( SortedMap )
        )
        .filter( _._2.abs > Tolerance )
        .partition( _._2 < 0 )

    ( in.map { case ( item, amount ) => ( item, -amount ) }, out )
  }

  def itemDiff( item: Item, before: Option[Double], after: Option[Double], changeValue: ChangeValue ): Option[Tag] = {

    val diff = after.orEmpty - before.orEmpty

    Option.when( diff.abs > Tolerance )(
      tr(
        td( item.displayName, fontWeight.bold ),
        td( before.fold( "-" )( d => f"$d%.3f" ) ),
        td( styleDiff( diff, changeValue ) ),
        td( after.fold( "-" )( d => f"$d%.3f" ) )
      )
    )

  }

  def itemMapDiff(
      before: SortedMap[Item, Double],
      after: SortedMap[Item, Double],
      changeValue: ChangeValue
  ): Frag = {
    val lines = before.keySet
      .union( after.keySet )
      .toVector
      .flatMap( item => itemDiff( item, before.get( item ), after.get( item ), changeValue ) )

    if (lines.isEmpty)
      td( colspan := 4, textAlign.center, "No difference" )
    else
      lines
  }

  def inputOutputsDiff( before: Factory, after: Factory ): Frag = {

    val ( inputsBefore, outputsBefore ) = inputOutputs( before )
    val ( inputsAfter, outputsAfter )   = inputOutputs( after )

    Seq(
      fieldset(
        legend( "Outputs" ),
        table( itemMapDiff( outputsBefore, outputsAfter, HigherIsBetter ) )
      ),
      fieldset(
        legend( "Inputs" ),
        table( itemMapDiff( inputsBefore, inputsAfter, LowerIsBetter ) )
      )
    )
  }

  ///////////////////////////////////
  // Recipes

  def recipeDiff( model: Model, before: Factory, after: Factory ): Tag = {
    val ( changed, unchanged ) =
      model.manufacturingRecipes
        .groupBy( _.products.head.item )
        .toVector
        .sortBy( _._1 )
        .foldMap {
          case ( item, recipes ) =>
            val recipeBefore = before.manufacturing.map( _.item ).find( recipes.contains )
            val recipeAfter  = after.manufacturing.map( _.item ).find( recipes.contains )
            if (recipeBefore == recipeAfter)
              ( Vector.empty, recipeAfter.map( ( item, _ ) ).toVector )
            else
              ( Vector( ( item, recipeBefore, recipeAfter ) ), Vector.empty )
        }

    fieldset(
      legend( "Recipes" ),
      table( changed.map {
        case ( item, recipeBefore, recipeAfter ) =>
          tr(
            td( textAlign.left, fontWeight.bold, item.displayName ),
            recipeBefore.fold[Frag]( td( colspan := 2 ) )( FactoryView.Blocks.recipeCell2Cols ),
            td( "\u25b6" ),
            recipeAfter.fold[Frag]( td( colspan := 2 ) )( FactoryView.Blocks.recipeCell2Cols )
          )
      } ),
      details(
        summary( "Unchanged" ),
        table( unchanged.map {
          case ( item, recipe ) =>
            tr(
              td( textAlign.left, fontWeight.bold, item.displayName ),
              FactoryView.Blocks.recipeCell2Cols( recipe )
            )
        } )
      )
    )
  }

  ////////////////////////////////////////
  // Style

  def styleDiff( diff: Double, changeValue: ChangeValue ): Tag =
    if (diff > 0 == (changeValue == HigherIsBetter)) Better( diff ) else Worse( diff )

  sealed abstract class ChangeValue
  final case object HigherIsBetter extends ChangeValue
  final case object LowerIsBetter  extends ChangeValue

  sealed abstract class DiffStyle( bc: String, c: String ) {
    def apply( diff: Double ): Tag =
      span(
        backgroundColor := bc,
        color := c,
        if (diff.abs < Tolerance) "=" else if (diff > 0) "\u25b2" else "\u25bc",
        " ",
        f"$diff%+.3f"
      )

  }
  object Better extends DiffStyle( "#D8E4BC", "#4F6228" )
  object Worse  extends DiffStyle( "#FCD5B4", "#E26B0A" )

}
