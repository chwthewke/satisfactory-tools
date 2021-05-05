package net.chwthewke.satisfactorytools
package web.view

import cats.syntax.foldable._
import scalatags.Text
import scalatags.Text.all._

import model.Bill
import model.Countable
import model.MapOptions
import model.Model
import model.Options
import model.RecipeList
import model.ResourcePurity
import web.protocol.Forms
import web.state.PageState

object AllInputsView {

  def apply( model: Model, state: PageState, stateBase64: Option[String] ): Text.TypedTag[String] =
    form(
      action := "/upgrade",
      method := "POST",
      enctype := "application/x-www-form-urlencoded",
      input( `type` := "submit", value := "Upgrade" ),
      bill( state.inputs.bill ),
      recipeList( state.inputs.recipeList ),
      options( state.inputs.options ),
      mapOptions( state.inputs.mapOptions ),
      inputTabs( state ),
      outputTabs( state ),
      customGroupRadios( model, state ),
      input( `type` := "hidden", name := Forms.state, stateBase64.map( value := _ ) )
    )

  def bill( bill: Bill ): Text.TypedTag[String] =
    div( bill.items.map {
      case Countable( item, amount ) =>
        input( `type` := "hidden", name := Forms.billItem( item ), value := amount )
    } )

  def recipeList( recipes: RecipeList ): Text.TypedTag[String] =
    div(
      recipes.recipes.map(
        recipe => input( `type` := "hidden", name := Forms.recipes, value := recipe.className.name )
      )
    )

  def options( opt: Options ): Text.TypedTag[String] =
    div(
      input( `type` := "hidden", name := Forms.optionsBeltKey, value := opt.belt.entryName ),
      input( `type` := "hidden", name := Forms.optionsPipeKey, value := opt.pipe.entryName ),
      input( `type` := "hidden", name := Forms.optionsMinerKey, value := opt.miner.entryName ),
      input( `type` := "hidden", name := Forms.optionsClockKey, value := opt.clockSpeed.entryName ),
      opt.extractors
        .map(
          extractor => input( `type` := "hidden", name := Forms.optionsExtractorsKey, value := extractor.entryName )
        )
        .toSeq,
      opt.preferFracking
        .map( fracking => input( `type` := "hidden", name := Forms.optionsFrackingKey, value := fracking.entryName ) )
        .toSeq
    )

  def mapOptions( mapOpt: MapOptions ): Text.TypedTag[String] =
    div(
      (for {
        ( exT, byItem )   <- mapOpt.resourceNodes
        ( item, distrib ) <- byItem
        purity            <- ResourcePurity.values
      } yield input(
        `type` := "hidden",
        name := Forms.extractorItemPurityKey( exT, item, purity ),
        value := distrib.get( purity )
      )).toSeq
    )

  def inputTabs( state: PageState ): Text.TypedTag[String] =
    input( `type` := "hidden", name := "input_tab", value := state.selectedInputTab.id )

  def outputTabs( state: PageState ): Text.TypedTag[String] =
    input( `type` := "hidden", name := "output_tab", value := state.selectedOutputTab.id )

  def customGroupRadios( model: Model, state: PageState ): Text.TypedTag[String] =
    div(
      state.factory.foldMap(
        _.foldMap(
          _.manufacturing.flatMap(
            block =>
              state.customGroupSelection.customGroups
                .get( block.item )
                .map(
                  ix =>
                    input(
                      `type` := "hidden",
                      name := Forms.outputGroup( model, block.item ),
                      value := ix
                    )
                )
          )
        )
      )
    )

}
