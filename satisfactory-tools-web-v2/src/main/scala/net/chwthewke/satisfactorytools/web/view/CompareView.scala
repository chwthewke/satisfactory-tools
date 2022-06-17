package net.chwthewke.satisfactorytools
package web.view

import cats.Order.catsKernelOrderingForOrder
import cats.data.Ior
import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.option._
import cats.syntax.semigroup._
import cats.syntax.traverse._
import scala.collection.immutable.SortedMap
import scalatags.Text

import data.Countable
import data.Item
import model.Model
import model.Recipe
import prod.ClockedRecipe
import prod.Factory
import protocol.ItemIO
import protocol.ItemSrcDest

object CompareView {
  import Text.all._
  import Text.tags2.details
  import Text.tags2.summary

  def apply(
      model: Model,
      before: Option[( Factory, Map[Item, ItemIO] )],
      after: Option[( Factory, Map[Item, ItemIO] )]
  ): Tag =
    page(
      "Compare plans",
      ( before, after )
        .mapN {
          case ( ( b, bio ), ( a, aio ) ) =>
            div(
              inputOutputsDiff( b, a ),
              recipeDiff2( b, a ),
              itemIODiff( bio, aio )
            )
        }
        .getOrElse( div( "Missing inputs or solution(s)" ) )
    )

  ////////////////////////////
  // Inputs/Outputs

  def itemIODiff( before: Map[Item, ItemIO], after: Map[Item, ItemIO] ): Tag = {
    val byItem: Vector[( Item, Option[ItemIO], Option[ItemIO] )] =
      (before.keySet ++ after.keySet).toVector.sorted
        .map( item => ( item, before.get( item ), after.get( item ) ) )

    fieldset(
      legend( "Items I/O" ),
      byItem.map {
        case ( item, beforeIO, afterIO ) => fieldset( legend( item.displayName ), compareItemIO( beforeIO, afterIO ) )
      }
    )
  }

  def compareItemIO( before: Option[ItemIO], after: Option[ItemIO] ): Frag = {
    def amountCells( beforeAmount: Option[Double], afterAmount: Option[Double], changeValue: ChangeValue ): Modifier = {
      val diff = afterAmount.orEmpty - beforeAmount.orEmpty

      if (diff.abs > AmountTolerance)
        Seq[Frag](
          td( beforeAmount.fold( "-" )( d => f"$d%.3f" ) ),
          td( styleDiffD3( diff, changeValue ) ),
          td( afterAmount.fold( "-" )( d => f"$d%.3f" ) )
        )
      else
        td( beforeAmount.fold( "-" )( d => f"$d%.3f" ), colspan := 3 )
    }

    def diffAllIO(
        f: ItemIO => Vector[Countable[Double, ItemSrcDest]],
        dir: String,
        changeValue: ChangeValue
    ): Seq[Tag] = {
      val allIO: Vector[ItemSrcDest] =
        (before.foldMap( i => f( i ).map( _.item ) ) ++ after.foldMap( i => f( i ).map( _.item ) )).distinct.sorted

      def amountOf( isd: ItemSrcDest, in: Option[ItemIO] ): Option[Double] =
        in.flatMap( i => f( i ).find( _.item == isd ).map( _.amount ) )

      allIO.zipWithIndex.map {
        case ( isd, ix ) =>
          val beforeAmount: Option[Double] = amountOf( isd, before )
          val afterAmount: Option[Double]  = amountOf( isd, after )

          tr(
            amountCells( beforeAmount, afterAmount, changeValue ),
            Option.when( ix == 0 )( td( rowspan := allIO.size, verticalAlign.middle, dir ) ),
            td( ItemsView.showItemSrcDest( isd ) )
          )
      }
    }

    val totalBefore = before.map( i => i.sources.foldMap( _.amount ) )
    val totalAfter  = after.map( i => i.sources.foldMap( _.amount ) )

    table(
      tr( amountCells( totalBefore, totalAfter, LowerIsBetter ), td(), td( strong( "TOTAL" ) ) ),
      diffAllIO( _.sources, "from", LowerIsBetter ) ++
        diffAllIO( _.destinations, "to", HigherIsBetter )
    )
  }

  def inputOutputs( factory: Factory ): ( SortedMap[Item, Double], SortedMap[Item, Double] ) = {
    val ( in, out ) =
      factory.manufacturing
        .foldMap(
          cr =>
            cr.flatTraverse( rec => rec.itemsPerMinute )
              .map { case Countable( item, amount ) => ( item, amount ) }
              .to( SortedMap )
        )
        .filter( _._2.abs > AmountTolerance )
        .partition( _._2 < 0 )

    ( in.map { case ( item, amount ) => ( item, -amount ) }, out )
  }

  def itemDiff( item: Item, before: Option[Double], after: Option[Double], changeValue: ChangeValue ): Option[Tag] = {

    val diff = after.orEmpty - before.orEmpty

    Option.when( diff.abs > AmountTolerance )(
      tr(
        td( item.displayName, fontWeight.bold ),
        td( before.fold( "-" )( d => f"$d%.3f" ) ),
        td( styleDiffD3( diff, changeValue ) ),
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
            recipeBefore.fold[Frag]( td( colspan := 2 ) )( StepsView.recipeCell2Cols ),
            td( "\u25b6" ),
            recipeAfter.fold[Frag]( td( colspan := 2 ) )( StepsView.recipeCell2Cols )
          )
      } ),
      details(
        summary( "Unchanged" ),
        table( unchanged.map {
          case ( item, recipe ) =>
            tr(
              td( textAlign.left, fontWeight.bold, item.displayName ),
              StepsView.recipeCell2Cols( recipe )
            )
        } )
      )
    )
  }

  def recipeDiff2( before: Factory, after: Factory ): Frag = {
    val changesByRecipe: Map[Recipe, Ior[Double, Double]] =
      before.manufacturing
        .groupMapReduce( _.item )( _.amount )( _ + _ )
        .map { case ( k, v ) => ( k, Ior.left[Double, Double]( v ) ) } |+|
        after.manufacturing
          .groupMapReduce( _.item )( _.amount )( _ + _ )
          .map { case ( k, v ) => ( k, Ior.right[Double, Double]( v ) ) }

    val ( removedRecipes, addedRecipes, countChangedRecipes, clockChangedRecipes, unchangedRecipes ) =
      changesByRecipe.toVector
        .map {
          case ( r, d ) =>
            d.bimap(
              x => ClockedRecipe.roundUp( Countable( r, x ) ),
              x => ClockedRecipe.roundUp( Countable( r, x ) )
            )

        }
        .foldMap {
          _.fold(
            b => ( Vector( b ), Vector.empty, Vector.empty, Vector.empty, Vector.empty ),
            a => ( Vector.empty, Vector( a ), Vector.empty, Vector.empty, Vector.empty ),
            ( b, a ) =>
              if ((a.fractionalAmount - b.fractionalAmount).abs < AmountTolerance)
                ( Vector.empty, Vector.empty, Vector.empty, Vector.empty, Vector( a ) )
              else if (b.machineCount != a.machineCount)
                ( Vector.empty, Vector.empty, Vector( ( b, a ) ), Vector.empty, Vector.empty )
              else
                ( Vector.empty, Vector.empty, Vector.empty, Vector( ( b, a ) ), Vector.empty )
          )
        }

    def machineCountCell( recipe: ClockedRecipe ): Tag =
      td( f"${recipe.machineCount}%3d", textAlign.right )

    def clockSpeedCell( recipe: ClockedRecipe ): Tag =
      td( f"${recipe.clockSpeedMillionth / 10000}%3d.${recipe.clockSpeedMillionth % 10000}%04d %%", textAlign.left )

    def recipeRow( recipe: ClockedRecipe ): Tag =
      tr(
        machineCountCell( recipe ),
        td( "@" ),
        clockSpeedCell( recipe ),
        StepsView.recipeCell2Cols( recipe.recipe.item )
      )

    def recipeClockChange( before: ClockedRecipe, after: ClockedRecipe ): Tag =
      tr(
        machineCountCell( before ),
        td( "@" ),
        clockSpeedCell( before ),
        td( styleDiffD4( after.clockSpeed - before.clockSpeed, LowerIsBetter ) ),
        clockSpeedCell( after ),
        StepsView.recipeCell2Cols( before.recipe.item )
      )

    def recipeCountChange( before: ClockedRecipe, after: ClockedRecipe ): Tag =
      tr(
        machineCountCell( before ),
        td( styleDiffI( after.machineCount - before.machineCount, LowerIsBetter ) ),
        machineCountCell( after ),
        td( "@" ),
        clockSpeedCell( before ),
        td( styleDiffD4( after.clockSpeed - before.clockSpeed, LowerIsBetter ) ),
        clockSpeedCell( after ),
        StepsView.recipeCell2Cols( before.recipe.item )
      )

    Seq[Frag](
      Option.when( removedRecipes.nonEmpty )(
        fieldset(
          legend( "Removed recipes" ),
          table( removedRecipes.map( recipeRow ) )
        )
      ),
      Option.when( addedRecipes.nonEmpty )(
        fieldset(
          legend( "New recipes" ),
          table( addedRecipes.map( recipeRow ) )
        )
      ),
      Option.when( clockChangedRecipes.nonEmpty )(
        fieldset(
          legend( "Recipe clock changes" ),
          table( clockChangedRecipes.map { case ( b, a ) => recipeClockChange( b, a ) } )
        )
      ),
      Option.when( countChangedRecipes.nonEmpty )(
        fieldset(
          legend( "Recipe count changes" ),
          table( countChangedRecipes.map { case ( b, a ) => recipeCountChange( b, a ) } )
        )
      ),
      Option.when( unchangedRecipes.nonEmpty )(
        details(
          summary( "Unchanged" ),
          table( unchangedRecipes.map( recipeRow ) )
        )
      )
    )

  }

  ////////////////////////////////////////
  // Style

  def styleDiffD3( diff: Double, changeValue: ChangeValue ): Tag =
    if (diff > 0 == (changeValue == HigherIsBetter)) Better.applyD3( diff ) else Worse.applyD3( diff )

  def styleDiffD4( diff: Double, changeValue: ChangeValue ): Tag =
    if (diff > 0 == (changeValue == HigherIsBetter)) Better.applyD4( diff ) else Worse.applyD4( diff )

  def styleDiffI( diff: Int, changeValue: ChangeValue ): Tag =
    if (diff > 0 == (changeValue == HigherIsBetter)) Better.applyI( diff ) else Worse.applyI( diff )

  sealed abstract class ChangeValue
  final case object HigherIsBetter extends ChangeValue
  final case object LowerIsBetter  extends ChangeValue

  sealed abstract class DiffStyle( bc: String, c: String ) {
    def applyD3( diff: Double ): Tag =
      span(
        backgroundColor := bc,
        color := c,
        if (diff.abs < AmountTolerance) "=" else if (diff > 0) "\u25b2" else "\u25bc",
        " ",
        f"$diff%+.3f"
      )

    def applyD4( diff: Double ): Tag =
      span(
        backgroundColor := bc,
        color := c,
        if (diff.abs < AmountTolerance) "=" else if (diff > 0) "\u25b2" else "\u25bc",
        " ",
        f"$diff%+.4f"
      )

    def applyI( diff: Int ): Tag =
      span(
        backgroundColor := bc,
        color := c,
        if (diff == 0) "=" else if (diff > 0) "\u25b2" else "\u25bc",
        " ",
        f"$diff%+d"
      )

  }

  object Better extends DiffStyle( "#D8E4BC", "#4F6228" )
  object Worse  extends DiffStyle( "#FCD5B4", "#E26B0A" )

}
