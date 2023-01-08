package net.chwthewke.satisfactorytools
package web.view

import scalatags.Text
import scalatags.Text.Tag

import model.GroupAssignments
import protocol.CustomGroupResult
import web.view.StepsView.CustomGroupsRadios

object CustomGroupView extends ( ( CustomGroupResult, Int ) => Tag ) {

  import Text.all._

  override def apply( result: CustomGroupResult, groupCount: Int ): Tag =
    fieldset(
      legend( s"Custom Group ${result.index}" ),
      StepsView.recipeTable( result.subFactory, GroupAssignments.empty, groupCount, CustomGroupsRadios.Sorting ),
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
