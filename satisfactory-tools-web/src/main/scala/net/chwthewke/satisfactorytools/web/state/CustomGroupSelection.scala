package net.chwthewke.satisfactorytools
package web.state

import model.Recipe

case class CustomGroupSelection( count: Int, customGroups: Map[Recipe, Int] ) {

  def canAdd: Boolean    = count < CustomGroupSelection.maxCustomGroups
  def canRemove: Boolean = count > 0 && customGroups.values.forall( _ < count )

}

object CustomGroupSelection {

  val defaultCustomGroups: Int = 8
  val maxCustomGroups: Int     = 15

  val empty: CustomGroupSelection = CustomGroupSelection( defaultCustomGroups, Map.empty )
}
