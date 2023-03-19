package net.chwthewke.satisfactorytools
package model

import data.ClassName

// TODO simplify by considering no group assignment as group #0
final case class GroupAssignments( groupsByClass: Map[ClassName, Int] ) extends AnyVal {
  def get( className: ClassName ): Option[Int]               = groupsByClass.get( className )
  def getOrElse( className: ClassName, orElse: => Int ): Int = groupsByClass.getOrElse( className, orElse )
}

object GroupAssignments {
  val empty: GroupAssignments = GroupAssignments( Map.empty )
}
