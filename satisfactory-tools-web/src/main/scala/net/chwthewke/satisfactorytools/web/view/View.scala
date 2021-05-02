package net.chwthewke.satisfactorytools
package web.view

import cats.syntax.option._
import scalatags.Text
import scalatags.Text.all._
import scalatags.Text.tags2.details
import scalatags.Text.tags2.summary

import model.Model
import web.protocol.Forms
import web.state.InputTab
import web.state.OutputTab
import web.state.PageState

object View {
  val pageStyle: Text.TypedTag[String] = Text.tags2.style(
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
  ): Text.TypedTag[String] = {
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
            stateBase64.map( b => a( "Bookmark", href := s"?state=$b" ) )
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

  def inputForm( model: Model, state: PageState ): Text.TypedTag[String] =
    div(
      id := "input",
      inputTabs( state.selectedInputTab ),
      state.selectedInputTab.view( model, state.selectedInputTab.stateLens.get( state.inputs ) )
    )

  def inputTabs( selectedTab: InputTab ): Text.TypedTag[String] =
    div(
      Vector(
        ( "Requested", InputTab.BillTab ),
        ( "Recipes", InputTab.RecipesTab ),
        ( "Resource nodes", InputTab.MapOptionsTab ),
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

  def outputForm( model: Model, state: PageState ): Text.TypedTag[String] =
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

  def outputTabs( state: PageState ): Text.TypedTag[String] =
    div(
      input(
        `type` := "submit",
        formaction := "/",
        value := state.factory.fold( "Compute" )( _ => "Recompute" )
      ),
      Vector(
        ( "Production steps", OutputTab.BlocksTab ),
        ( "Raw resources", OutputTab.ResourcesTab ),
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
      1.to( 5 ).map { ix =>
        input(
          `type` := "submit",
          formaction := s"/output/group$ix",
          value := ix,
          Option.when( state.selectedOutputTab == OutputTab.CustomGroup( ix ) )( fontWeight := "bold" )
        )

      }
    )

}
