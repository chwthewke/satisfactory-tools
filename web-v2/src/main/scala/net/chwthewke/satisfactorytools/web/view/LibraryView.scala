package net.chwthewke.satisfactorytools
package web.view

import cats.syntax.all._
import java.time.ZoneId
import java.time.format.DateTimeFormatter
import scalatags.Text.Tag
import scalatags.Text.all._

import model.ModelVersion
import protocol.ModelVersionId
import protocol.PlanHeader
import protocol.PlanName
import web.forms

object LibraryView {

  private def planDisplayName( title: Option[PlanName] ): PlanName =
    title.getOrElse( PlanName( "(Untitled plan)" ) )

  def viewAllPlans( plans: Vector[PlanHeader], modelVersions: Vector[( ModelVersionId, ModelVersion )] ): Tag =
    page(
      "Satisfactory Planner",
      form(
        method := "POST",
        table(
          id      := "library",
          `class` := "table is-striped",
          tr(
            td( colspan := 5, textAlign.left, show"${plans.size} plan${if (plans.size != 1) "s" else ""}" )
          ),
          tr(
            td(
              colspan := 3,
              select(
                name := forms.Keys.modelVersion,
                modelVersions.map {
                  case ( id, ModelVersion( _, name ) ) =>
                    option( value := id.show, name )
                }
              ),
              button(
                `class`    := "button is-success",
                formaction := "/new",
                "New plan"
              )
            ),
            td(
              colspan := 2,
              button(
                `class`    := "button is-info",
                formaction := "/compare",
                "Compare"
              )
            )
          ),
          plans.map( plan =>
            tr(
              td( a( href := show"/plan/${plan.id}", planDisplayName( plan.title ).show ) ),
              td( plan.updated.atZone( ZoneId.systemDefault() ).format( DateTimeFormatter.RFC_1123_DATE_TIME ) ),
              td(
                button(
                  `class`    := "button is-danger",
                  formaction := show"/delete/${plan.id}/",
                  "Delete"
                )
              ),
              td( input( `type` := "radio", name := forms.Keys.compareBefore, value := show"${plan.id}" ) ),
              td( input( `type` := "radio", name := forms.Keys.compareAfter, value := show"${plan.id}" ) )
            )
          )
        )
      )
    )

  def deleteConfirm( planHeader: PlanHeader ): Tag =
    page(
      "Confirmation needed",
      div(
        alignSelf.center,
        width.auto,
        form( method := "POST" ),
        h4( "Confirmation needed" ),
        p(
          show"Delete plan ${planHeader.title.fold( "(Untitled)" )( _.show )} ?",
          br(),
          s"It was last modified on ${planHeader.updated}."
        ),
        form(
          method := "POST",
          div(
            alignContent.center,
            button(
              `class`    := "button",
              formaction := "confirm",
              "Yes"
            ),
            button(
              `class`    := "button",
              formaction := "cancel",
              "No"
            )
          )
        )
      )
    )
}
