package net.chwthewke.satisfactorytools
package web.view

import cats.syntax.foldable._
import scalatags.Text

import data.Countable
import model.Bill
import model.ResourceOptions
import model.Model
import model.Options
import model.RecipeList
import model.ResourcePurity
import model.ResourceWeights
import web.protocol.Forms
import web.state.PageState

object AllInputsView {
  import Text.all._

  def apply( model: Model, state: PageState, stateBase64: Option[String] ): Tag =
    form(
      action := "/upgrade",
      method := "POST",
      enctype := "application/x-www-form-urlencoded",
      input( `type` := "submit", value := "Upgrade" ),
      bill( state.inputs.bill ),
      recipeList( state.inputs.recipeList ),
      options( state.inputs.options ),
      resourceOptions( model, state.inputs.resourceOptions ),
      inputTabs( state ),
      outputTabs( state ),
      input( `type` := "hidden", name := Forms.outputGroupCount, value := state.customGroupSelection.count ),
      customGroupRadios( model, state ),
      input( `type` := "hidden", name := Forms.state, stateBase64.map( value := _ ) )
    )

  def bill( bill: Bill ): Tag =
    div( bill.items.map {
      case Countable( item, amount ) =>
        input( `type` := "hidden", name := Forms.billItem( item ), value := amount )
    } )

  def recipeList( recipes: RecipeList ): Tag =
    div(
      recipes.recipes.map(
        recipe => input( `type` := "hidden", name := Forms.recipes, value := recipe.className.name )
      )
    )

  def options( opt: Options ): Tag =
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

  def resourceOptions( model: Model, resourceOpts: ResourceOptions ): Tag =
    div(
      (for {
        ( exT, byItem )   <- resourceOpts.resourceNodes
        ( item, distrib ) <- byItem
        purity            <- ResourcePurity.values
      } yield input(
        `type` := "hidden",
        name := Forms.extractorItemPurityKey( exT, item, purity ),
        value := distrib.get( purity )
      )).toSeq,
      model.extractedItems.map(
        item =>
          input(
            `type` := "hidden",
            name := Forms.resourceWeightKey( item ),
            value := resourceOpts.resourceWeights.weights.getOrElse( item, ResourceWeights.range )
          )
      )
    )

  def inputTabs( state: PageState ): Tag =
    input( `type` := "hidden", name := "input_tab", value := state.selectedInputTab.id )

  def outputTabs( state: PageState ): Tag =
    input( `type` := "hidden", name := "output_tab", value := state.selectedOutputTab.id )

  def customGroupRadios( model: Model, state: PageState ): Tag =
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
