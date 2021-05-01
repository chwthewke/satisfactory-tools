package net.chwthewke.satisfactorytools
package web.view

import cats.syntax.functor._
import scalatags.Text
import scalatags.Text.all._
import scalatags.Text.tags2.details
import scalatags.Text.tags2.summary

import model.Model
import prod.Factory
import web.protocol.FormNames
import web.state.InputTab
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
      state: PageState,
      solution: Option[Either[String, Factory]] = None
  ): Text.TypedTag[String] = {
    val stateBase64 = PageState.toBase64( model, state )

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
              stateBase64
            ),
            solution.as( a( "Bookmark", href := s"?state=$stateBase64" ) )
          )
        ),
        div(
          id := "main",
          form(
            id := "input",
            action := "/",
            method := "POST",
            enctype := "application/x-www-form-urlencoded",
            div( input( `type` := "submit", value := "Go!" ) ),
            input( `type` := "hidden", name := FormNames.state, value := stateBase64 ),
            inputTabs( state.selectedInputTab ),
            state.selectedInputTab.view( model, state.selectedInputTab.stateLens.get( state.inputs ) )
          ),
          div( id := "output", solution.map( FactoryView( _ ) ) )
        )
      )
    )
  }

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
            formaction := s"/input/${tab.id}/from/${selectedTab.id}",
            value := text,
            Option.when( tab == selectedTab )( disabled )
          )
      }
    )

}
