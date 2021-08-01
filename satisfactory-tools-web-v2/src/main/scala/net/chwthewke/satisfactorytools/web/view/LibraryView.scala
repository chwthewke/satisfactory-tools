package net.chwthewke.satisfactorytools
package web.view

import cats.syntax.show._
import java.time.ZoneId
import scalatags.Text.Tag
import scalatags.Text.all._

import protocol.PlanHeader

object LibraryView {

  private def buttonNew: Tag =
    button(
      formaction := "/new",
      "New plan"
    )

  def viewAllPlans( plans: Vector[PlanHeader] ): Tag =
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
              td( a( href := show"/plan/${plan.id}", plan.title.fold( "(Untitled plan)" )( _.show ) ) ),
              td( plan.updated.atZone( ZoneId.systemDefault() ).toString )
            )
        )
      )
    )

}
