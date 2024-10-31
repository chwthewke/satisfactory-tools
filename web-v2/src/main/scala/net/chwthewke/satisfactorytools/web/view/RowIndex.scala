package net.chwthewke.satisfactorytools
package web
package view

final case class RowIndex( index: Int, total: Int ) {
  def canMoveUp: Boolean   = index > 0
  def canMoveDown: Boolean = index + 1 < total
}

object RowIndex {
  val zero: RowIndex = RowIndex( 0, 0 )
}
