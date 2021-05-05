package net.chwthewke.satisfactorytools
package web.view

import cats.Order.catsKernelOrderingForOrder
import cats.syntax.show._
import scalatags.Text
import scalatags.Text.all._

import model.Bill
import model.Countable
import model.Item
import model.MachineType
import model.Model
import web.protocol.Forms

object BillView {

  def view( model: Model, bill: Bill ): Text.TypedTag[String] = {
    val eligibleRecipes =
      model.manufacturingRecipes.filter( _.producedIn.machineType == MachineType.Manufacturer )

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

  def itemField( item: Countable[Double, Item] ): Text.TypedTag[String] = {
    val inputName = Forms.billItem( item.item )
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
