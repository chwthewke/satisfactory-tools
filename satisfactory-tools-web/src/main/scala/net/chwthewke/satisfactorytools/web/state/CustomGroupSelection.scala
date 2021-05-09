package net.chwthewke.satisfactorytools
package web.state

import model.Item
import model.Machine
import model.Recipe

case class CustomGroupSelection( customGroups: Map[Recipe[Machine, Item], Int] )

object CustomGroupSelection {
  val empty: CustomGroupSelection = CustomGroupSelection( Map.empty )
  val customGroups: Int           = 8
}
