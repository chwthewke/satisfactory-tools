package net.chwthewke.satisfactorytools
package web.view

import cats.syntax.functor._
import scalatags.Text
import scalatags.Text.all._

import model.Model
import model.SolverInputs
import prod.Factory
import web.protocol.SolverInputsCodec

object View {
  def apply(
      model: Model,
      defaultInputs: SolverInputs,
      inputs: SolverInputs,
      solution: Option[Either[String, Factory]] = None
  ): Text.TypedTag[String] =
    html(
      head( title := "Satisfactory Planner" ),
      body(
        fieldset(
          legend( "Debug" ),
          div(
            label( `for` := "state_text", "State" ),
            textarea(
              readonly,
              id := "state_text",
              wrap := "hard",
              cols := 120,
              rows := 10,
              SolverInputsCodec.toBase64( model, inputs )
            )
          )
        ),
        solution.as(
          div( a( "Bookmark", href := s"?state=${SolverInputsCodec.toBase64( model, inputs )}" ) )
        ),
        solution.map( FactoryView( _ ) ),
        form(
          action := "/",
          method := "POST",
          enctype := "application/x-www-form-urlencoded",
          div( input( `type` := "submit", value := "Go!" ) ),
          BillView.view( model, inputs.bill ),
          RecipeListView.view( model, inputs.recipeList ),
          MapOptionsView.view( defaultInputs.mapOptions, inputs.mapOptions ),
          OptionsView.view( inputs.options )
        )
      )
    )
}
