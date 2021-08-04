package net.chwthewke.satisfactorytools
package web.view

import cats.syntax.show._
import java.time.ZoneId
import scalatags.Text.Tag
import scalatags.Text.all._

import protocol.PlanHeader
import protocol.PlanName

object LibraryView {

  private def buttonNew: Tag =
    button(
      formaction := "/new",
      "New plan"
    )

  private def planDisplayName( title: Option[PlanName] ): PlanName =
    title.getOrElse( PlanName( "(Untitled plan)" ) )

  private def page( bodyContents: Tag ): Tag =
    html(
      head( title := "Satisfactory Planner", pageStyle ),
      body( bodyContents )
    )

  def viewAllPlans( plans: Vector[PlanHeader] ): Tag =
    page(
      form(
        method := "POST",
        table(
          tr(
            td( buttonNew ),
            td( textAlign.right, show"${plans.size} plans" )
          ),
          plans.map(
            plan =>
              tr(
                td( a( href := show"/plan/${plan.id}", planDisplayName( plan.title ).show ) ),
                td( plan.updated.atZone( ZoneId.systemDefault() ).toString ),
                td( a( href := show"/delete/${plan.id}/", "Delete" ) )
              )
          )
        )
      )
    )

  def deleteConfirm( planHeader: PlanHeader ): Tag =
    page(
      div(
        alignSelf.center,
        width.auto,
        form( method := "POST" ),
        h4( "Confirmation required" ),
        p( show"Delete plan ${planHeader.title} ?", br(), s"It was last modified on ${planHeader.updated}." ),
        form(
          method := "POST",
          div(
            alignContent.center,
            button(
              formaction := "confirm",
              "Yes"
            ),
            button(
              formaction := "cancel",
              "No"
            )
          )
        )
      )
    )
}
