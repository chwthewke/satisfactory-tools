package net.chwthewke.satisfactorytools
package web.view

import scalatags.Text
import scalatags.Text.Tag

import protocol.CustomGroupResult
import protocol.OutputTab
import web.forms.Actions
import web.view.StepsView.CustomGroupsRadios

case class CustomGroupView( editOrder: Boolean ) extends ( ( CustomGroupResult, Int ) => Tag ) {

  import Text.all._

  override def apply( result: CustomGroupResult, groupCount: Int ): Tag =
    fieldset(
      legend(
        s"Custom Group ${result.index}",
        raw( "&nbsp;" ),
        button(
          formaction := s"output/${Actions.output( OutputTab.CustomGroup( result.index, !editOrder ) )}",
          if (editOrder) "Done editing order" else "Edit order"
        )
      ),
      StepsView
        .recipeTable(
          result.subFactory,
          if (editOrder) CustomGroupsRadios.Sorting( result.groupAssignment )
          else CustomGroupsRadios.Group( result.groupAssignment )
        ),
      MachinesView( result.machines, groupCount ),
      fieldset(
        legend( "Inputs" ),
        InputsView.extractedResourcesView( result.subFactory.extraInputs )
      ),
      fieldset(
        legend( "Outputs" ),
        InputsView.extractedResourcesView( result.subFactory.extraOutputs )
      ),
      ItemsView( result.items, groupCount )
    )

}
