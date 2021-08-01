package net.chwthewke.satisfactorytools
package web.view

import cats.syntax.foldable._
import cats.syntax.show._
import scalatags.Text.Tag
import scalatags.Text.all._

import model.Model
import web.forms._
import protocol.InputTab
import protocol.OutputTab
import protocol.PlanHeader
import protocol.SolutionHeader

object PlanView {
  def apply[I, O](
      model: Model,
      header: PlanHeader,
      inputTab: InputTab.Aux[I],
      input: I,
      outputTab: OutputTab.Aux[O],
      output: SolutionHeader[O]
  ): Tag = html(
    head( title := header.title.mkString_( "Satisfactory Planner ", "", "" ), pageStyle ),
    body(
      form(
        method := "POST",
        enctype := "application/x-www-form-urlencoded",
        div(
          id := "main",
          div(
            id := "input",
            inputTabs( inputTab ),
            inputView( inputTab )( model, input )
          ),
          div(
            id := "output",
            outputTabs( outputTab, output ),
            output match {
              case SolutionHeader.NotComputed        => p( "Configure plan on the left and press Compute" )
              case SolutionHeader.PlanError( error ) => p( s"Error: $error" )
              case SolutionHeader.Computed( outputData, groupCount, _ ) =>
                outputView( outputTab )( outputData, groupCount )
            }
          )
        )
      )
    )
  )

  def inputTabs( selected: InputTab ): Tag =
    div(
      Vector(
        ( "Requested", InputTab.Bill ),
        ( "Recipes", InputTab.Recipes ),
        ( "Resource nodes", InputTab.ResourceOptions ),
        ( "Options", InputTab.Options )
      ).map {
        case ( text, tab ) =>
          button(
            formaction := s"input/${Actions.input( tab )}",
            text,
            Option.when( tab == selected )( fontWeight.bold )
          )
      }
    )

  def inputView[I]( inputTab: InputTab.Aux[I] ): ( Model, I ) => Tag =
    inputTab match {
      case InputTab.Bill            => BillView
      case InputTab.Recipes         => RecipesView
      case InputTab.Options         => OptionsView
      case InputTab.ResourceOptions => ResourceOptionsView
    }

  def outputTabs[X]( selected: OutputTab, solution: SolutionHeader[X] ): Tag = {
    val computeTab =
      button(
        formaction := "compute",
        if (solution.isComputed) "Recompute" else "Compute"
      )

    val regularTabs =
      (
        Vector(
          ( "Production steps", OutputTab.Steps ),
          ( "Raw resources", OutputTab.Inputs ),
          ( "Manufacturing machines", OutputTab.Machines ),
          ( "Item I/O", OutputTab.Items )
        ) ++
          (1 to solution.groupCount)
            .map( ix => ( ix.show, OutputTab.CustomGroup( ix ) ) )
      ).map {
        case ( text, tab ) =>
          button(
            formaction := s"output/${Actions.output( tab )}",
            Option.when( tab == selected )( fontWeight.bold ),
            text
          )
      }

    val groupActionTabs =
      Vector(
        button(
          formaction := "group_dec",
          Option.when( !solution.canRemoveGroup )( disabled ),
          "-"
        ),
        button(
          formaction := "group_inc",
          Option.when( !solution.canAddGroup )( disabled ),
          "+"
        )
      )

    div(
      computeTab,
      (regularTabs ++ groupActionTabs).filter( _ => solution.isComputed )
    )
  }

  def outputView[O]( outputTab: OutputTab.Aux[O] ): ( O, Int ) => Tag =
    outputTab match {
      case OutputTab.CustomGroup( _ ) => CustomGroupView
      case OutputTab.Steps            => StepsView
      case OutputTab.Items            => ItemsView
      case OutputTab.Machines         => MachinesView
      case OutputTab.Inputs           => InputsView
    }

}
