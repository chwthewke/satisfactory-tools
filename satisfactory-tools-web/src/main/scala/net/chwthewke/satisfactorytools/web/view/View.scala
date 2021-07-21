package net.chwthewke.satisfactorytools
package web.view

import cats.syntax.option._
import scalatags.Text

import model.Model
import web.protocol.Forms
import web.state.InputTab
import web.state.OutputTab
import web.state.PageState

object View {
  import Text.all._
  import Text.tags2.details
  import Text.tags2.summary

  val pageStyle: Tag = Text.tags2.style(
    // language=CSS
    """#main {
      |  display: flex;
      |  flex-flow: row;
      |}
      |
      |#input {
      |  flex: auto;
      |}
      |
      |#output {
      |  flex: auto;
      |}
      |
      |""".stripMargin
  )

  def tabbed(
      model: Model,
      state: PageState
  ): Tag = {
    val stateBase64 = PageState.toBase64( model, state ).toOption

    html(
      head( title := "Satisfactory Planner", pageStyle ),
      body(
        details(
          summary( "Debug" ),
          div(
            label( `for` := "state_text", "State" ),
            textarea(
              readonly,
              id := "state_text",
              wrap := "hard",
              cols := 120,
              rows := 10,
              stateBase64.orEmpty
            ),
            stateBase64.map( b => a( "Bookmark", href := s"?state=$b" ) ),
            AllInputsView( model, state, stateBase64 )
          )
        ),
        form(
          action := "/",
          method := "POST",
          enctype := "application/x-www-form-urlencoded",
          div(
            id := "main",
            input( `type` := "hidden", name := Forms.state, stateBase64.map( value := _ ) ),
            inputForm( model, state ),
            outputForm( model, state )
          )
        )
      )
    )
  }

  def inputForm( model: Model, state: PageState ): Tag =
    div(
      id := "input",
      inputTabs( state.selectedInputTab ),
      state.selectedInputTab.view( model, state.selectedInputTab.stateLens.get( state.inputs ) )
    )

  def inputTabs( selectedTab: InputTab ): Tag =
    div(
      Vector(
        ( "Requested", InputTab.BillTab ),
        ( "Recipes", InputTab.RecipesTab ),
        ( "Resource nodes", InputTab.ResourceOptionsTab ),
        ( "Options", InputTab.OptionsTab )
      ).map {
        case ( text, tab ) =>
          input(
            `type` := "submit",
            formaction := s"/input/${tab.id}",
            value := text,
            Option.when( tab == selectedTab )( fontWeight := "bold" )
          )
      }
    )

  def outputForm( model: Model, state: PageState ): Tag =
    div(
      id := "output",
      outputTabs( state ),
      state.factory.fold[Frag]( "" )(
        _.fold(
          err => p( s"Could not find a solution $err" ),
          factory => state.selectedOutputTab.view( model, state, factory )
        )
      )
    )

  def outputTabs( state: PageState ): Tag =
    div(
      input(
        `type` := "submit",
        formaction := "/",
        value := state.factory.fold( "Compute" )( _ => "Recompute" )
      ),
      Vector(
        ( "Production steps", OutputTab.BlocksTab ),
        ( "Raw resources", OutputTab.ResourcesTab ),
        ( "Manufacturing machines", OutputTab.MachinesTab ),
        ( "Item I/O", OutputTab.ItemsTab )
      ).map {
        case ( text, tab ) =>
          input(
            `type` := "submit",
            formaction := s"/output/${tab.id}",
            value := text,
            Option.when( tab == state.selectedOutputTab )( fontWeight := "bold" )
          )
      },
      1.to( state.customGroupSelection.count ).map { ix =>
        val groupTab = OutputTab.CustomGroup( ix )
        input(
          `type` := "submit",
          formaction := s"/output/${groupTab.id}",
          value := ix,
          Option.when( state.selectedOutputTab == groupTab )( fontWeight := "bold" )
        )
      },
      input(
        `type` := "submit",
        formaction := "/group_dec",
        value := "-",
        Option.when( !state.customGroupSelection.canRemove )( disabled )
      ),
      input(
        `type` := "submit",
        formaction := "/group_inc",
        value := "+",
        Option.when( !state.customGroupSelection.canAdd )( disabled )
      ),
      input( `type` := "hidden", name := Forms.outputGroupCount, value := state.customGroupSelection.count )
    )

}
