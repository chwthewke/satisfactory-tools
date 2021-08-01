package net.chwthewke.satisfactorytools
package web.view

import cats.Order.catsKernelOrderingForOrder
import cats.syntax.show._
import scalatags.Text
import scalatags.Text.Tag

import data.Countable
import data.Item
import model.Bill
import model.Model
import web.forms

object BillView extends ( ( Model, Bill ) => Tag ) {
  import Text.all._

  def apply( model: Model, bill: Bill ): Tag = {
    val eligibleRecipes =
      model.manufacturingRecipes

    val eligibleItems =
      model.items.values
        .filter( item => eligibleRecipes.exists( recipe => recipe.products.exists( _.item == item ) ) )
        .toVector

    fieldset(
      legend( "Production target (per minute)" ),
      table(
        thead( tr( th( "Item" ), th( "Amount" ) ) ),
        tbody(
          eligibleItems.sorted
            .map( item => bill.items.find( _.item == item ).getOrElse( Countable( item, 0d ) ) )
            .map( itemField )
        )
      )
    )
  }

  def itemField( item: Countable[Double, Item] ): Tag = {
    val inputName = forms.Keys.billItem( item.item )
    val elId      = show"input_$inputName"

    tr(
      td( label( `for` := elId, item.item.displayName ) ),
      td(
        input(
          `type` := "number",
          step := "any",
          min := 0,
          name := inputName,
          value := item.amount
        )
      )
    )
  }
}
