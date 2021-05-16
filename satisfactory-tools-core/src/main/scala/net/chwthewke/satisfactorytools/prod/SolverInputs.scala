package net.chwthewke.satisfactorytools
package prod

import cats.Show
import cats.syntax.show._

import model.Bill
import model.Options
import model.RecipeList
import model.ResourceOptions

case class SolverInputs(
    bill: Bill,
    recipeList: RecipeList,
    options: Options,
    resourceOptions: ResourceOptions
)

object SolverInputs {
  implicit val solverInputsShow: Show[SolverInputs] =
    Show.show(
      inputs => //
        show"""BILL
              |${inputs.bill}
              |
              |RECIPES
              |${inputs.recipeList}
              |
              |OPTIONS
              |${inputs.options}
              |
              |MAP
              |${inputs.resourceOptions}
              |""".stripMargin
    )
}
