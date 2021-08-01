package net.chwthewke.satisfactorytools
package protocol

import cats.Order
import cats.Show
import cats.derived.semiauto

import model.Recipe

sealed trait ItemSrcDest extends Product

object ItemSrcDest {
  final case class Step( recipe: Recipe ) extends ItemSrcDest
  final case object Input                 extends ItemSrcDest
  final case object Output                extends ItemSrcDest
  final case object Byproduct             extends ItemSrcDest
  final case object Requested             extends ItemSrcDest

  private def index( srcDest: ItemSrcDest ): ( Int, String ) = srcDest match {
    case Input          => ( -1, "" )
    case Step( recipe ) => ( 0, recipe.displayName )
    case Output         => ( 1, "" )
    case Requested      => ( 2, "" )
    case Byproduct      => ( 3, "" )
  }

  implicit val itemSrcDestShow: Show[ItemSrcDest]   = semiauto.show[ItemSrcDest]
  implicit val itemSrcDestOrder: Order[ItemSrcDest] = Order.by( index )
}
