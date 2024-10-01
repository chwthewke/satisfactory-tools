package net.chwthewke.satisfactorytools
package web.view

import cats.syntax.foldable._
import cats.syntax.show._
import scala.annotation.nowarn
import scalatags.Text.Tag
import scalatags.Text.all._

import model.Model
import model.ModelVersion
import protocol.InputTab
import protocol.OutputTab
import protocol.PlanHeader
import protocol.SolutionHeader
import web.forms._

object PlanView {
  def apply[I, O](
      model: Model,
      migrationTarget: Option[ModelVersion],
      header: PlanHeader,
      inputTab: InputTab.Aux[I],
      input: I,
      outputTab: OutputTab.Aux[O],
      output: SolutionHeader[O]
  ): Tag = page(
    header.title.mkString_( "Satisfactory Planner ", "", "" ),
    form(
      method  := "POST",
      enctype := "application/x-www-form-urlencoded",
      documentHeader( header, model.version, migrationTarget ),
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

  private def documentHeader(
      header: PlanHeader,
      modelVersion: ModelVersion,
      migrationTarget: Option[ModelVersion]
  ): Tag =
    div(
      id := "header",
      display.flex,
      flexDirection.row,
      flexWrap.nowrap,
      button( `class` := "button is-medium", "Library", formaction := "/library" ),
      input(
        `type` := "text",
        fontSize.`x-large`,
        Option.when( header.dirty )( color.gray ),
        value       := header.title.fold( "" )( _.show ),
        placeholder := "Untitled plan",
        name        := Keys.planTitle
      ),
      button(
        `class`    := "button is-success is-medium",
        formaction := Actions.save,
        "Save"
      ),
      div( flexGrow := "1" ),
      div(
        div( color.gray, modelVersion.name ),
        migrationTarget
          .filter( _ != modelVersion )
          .map( v =>
            button(
              `class`    := "button is-warning",
              formaction := Actions.migrate,
              s"Migrate to ${v.name}"
            )
          )
      )
    )

  private def inputTabs( selected: InputTab ): Tag =
    div(
      Vector(
        ( "Requested", InputTab.Bill ),
        ( "Recipes", InputTab.Recipes ),
        ( "Resource nodes", InputTab.ResourceOptions ),
        ( "Options", InputTab.Options )
      ).map {
        case ( text, tab ) =>
          button(
            `class`    := "button is-small",
            formaction := s"input/${Actions.input( tab )}",
            text,
            Option.when( tab == selected )( fontWeight.bold )
          )
      }
    )

  private def inputView[I]( inputTab: InputTab.Aux[I] ): ( Model, I ) => Tag =
    inputTab match {
      case InputTab.Bill            => BillView
      case InputTab.Recipes         => RecipesView
      case InputTab.Options         => OptionsView
      case InputTab.ResourceOptions => ResourceOptionsView
    }

  private def outputTabs[X]( selected: OutputTab, solution: SolutionHeader[X] ): Tag = {
    val computeTab =
      button(
        `class`    := "button is-small",
        formaction := Actions.compute,
        if (solution.isComputed) "Recompute" else "Compute"
      )

    val regularTabs: Vector[Tag] =
      (
        Vector(
          ( "Production steps", OutputTab.Steps( selected.editMode ) ),
          ( "Raw resources", OutputTab.Inputs ),
          ( "Manufacturing machines", OutputTab.Machines ),
          ( "Item I/O", OutputTab.Items ),
          ( "Inter-group Item I/O", OutputTab.GroupIO ),
          ( "Tree (beta)", OutputTab.Tree )
        ) ++
          ( 1 to solution.groupCount )
            .map( ix => ( ix.show, OutputTab.CustomGroup( ix, selected.editMode ) ) )
      ).map {
        case ( text, tab ) =>
          button(
            `class`    := "button is-small",
            formaction := s"output/${Actions.output( tab )}",
            Option.when( tab == selected )( fontWeight.bold ),
            text
          )
      }

    val groupActionTabs: Vector[Tag] =
      Vector(
        button(
          `class`    := "button is-small",
          formaction := Actions.removeGroup,
          Option.when( !solution.canRemoveGroup )( disabled ),
          "-"
        ),
        button(
          `class`    := "button is-small",
          formaction := Actions.addGroup,
          Option.when( !solution.canAddGroup )( disabled ),
          "+"
        )
      )

    div(
      computeTab,
      ( regularTabs ++ groupActionTabs ).filter( _ => solution.isComputed )
    )
  }

  @nowarn( "msg=unreachable code" )
  private def outputView[O]( outputTab: OutputTab.Aux[O] ): ( O, Int ) => Tag =
    outputTab match {
      case OutputTab.CustomGroup( _, editOrder ) => CustomGroupView( editOrder )
      case OutputTab.GroupIO                     => GroupIOView
      case OutputTab.Steps( editGroups )         => StepsView( editGroups )
      case OutputTab.Items                       => ItemsView
      case OutputTab.Machines                    => MachinesView
      case OutputTab.Inputs                      => InputsView
      case OutputTab.Tree                        => TreeView
    }

}
