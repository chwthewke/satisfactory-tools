package net.chwthewke.satisfactorytools
package protocol

import cats.Order
import cats.Show
import cats.derived.semiauto

import model.Recipe

sealed trait ItemSrcDest extends Product

object ItemSrcDest {
  final case class Step( recipe: Recipe )  extends ItemSrcDest
  final case class FromGroup( group: Int ) extends ItemSrcDest
  final case class ToGroup( group: Int )   extends ItemSrcDest
  final case object Input                  extends ItemSrcDest
  final case object Output                 extends ItemSrcDest
  final case object Byproduct              extends ItemSrcDest
  final case object Requested              extends ItemSrcDest

  private def index( srcDest: ItemSrcDest ): ( Int, Int, String ) = srcDest match {
    case Input          => ( -1, 0, "" )
    case FromGroup( g ) => ( -1, g, "" )
    case Step( recipe ) => ( 0, 0, recipe.displayName )
    case Output         => ( 1, 0, "" )
    case ToGroup( g )   => ( 1, g, "" )
    case Requested      => ( 2, 0, "" )
    case Byproduct      => ( 3, 0, "" )
  }

  implicit val itemSrcDestShow: Show[ItemSrcDest]   = semiauto.show[ItemSrcDest]
  implicit val itemSrcDestOrder: Order[ItemSrcDest] = Order.by( index )
}
