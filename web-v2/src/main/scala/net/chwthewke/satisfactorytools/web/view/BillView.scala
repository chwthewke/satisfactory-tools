package net.chwthewke.satisfactorytools
package web.view

import cats.syntax.foldable._
import cats.syntax.show._
import scalatags.Text
import scalatags.Text.Tag

import data.Countable
import data.Form
import data.Item
import model.Bill
import model.Model
import model.Recipe
import web.forms

object BillView extends ( ( Model, Bill ) => Tag ) {
  import Text.all._

  def apply( model: Model, bill: Bill ): Tag = {
    val eligibleRecipes: Vector[Recipe] =
      model.manufacturingRecipes

    val eligibleItems: Vector[Item] =
      model.items.values
        .filter( item => eligibleRecipes.exists( recipe => recipe.products.exists( _.item == item ) ) )
        .toVector

    val items: Vector[Countable[Double, Item]] =
      eligibleItems.sorted.map( item => bill.items.find( _.item == item ).getOrElse( Countable( item, 0d ) ) )

    val ( ( count, total ), ( countE, totalE ) ) =
      items
        .filter( _.amount > AmountTolerance )
        .foldMap { it =>
          val counted: ( Int, Double ) = ( 1, it.amount )

          if (
            it.item.className.name.startsWith( "Desc_SpaceElevatorPart_" ) ||
            it.item.form != Form.Solid
          ) ( counted, ( 0, 0d ) )
          else ( counted, counted )
        }

    fieldset(
      legend( "Production target (per minute)" ),
      table(
        thead( tr( th( "Item" ), th( "Amount" ) ) ),
        tbody( items.map( itemField ) ),
        tfoot(
          tr( td( colspan := "2", s"$count items requested, $total/min total." ) ),
          tr( td( colspan := "2", "Excluding fluids and project parts:" ) ),
          tr( td( colspan := "2", s"$countE items requested, $totalE/min total." ) )
        )
      )
    )
  }

  def itemField( item: Countable[Double, Item] ): Tag = {
    val inputName = forms.Keys.billItem( item.item )
    val elId      = show"input_$inputName"

    tr(
      td(
        label( `for` := elId, item.item.displayName, Option.when( item.amount > AmountTolerance )( fontWeight.bold ) )
      ),
      td(
        input(
          `type` := "number",
          step   := "any",
          min    := 0,
          name   := inputName,
          value  := item.amount
        )
      )
    )
  }
}
