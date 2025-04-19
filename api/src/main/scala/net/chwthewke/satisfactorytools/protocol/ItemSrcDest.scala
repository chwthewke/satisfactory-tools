package net.chwthewke.satisfactorytools
package protocol

import cats.Order
import cats.Show
import cats.derived.semiauto

import model.Recipe

sealed trait ItemSrcDest extends Product

object ItemSrcDest {
  sealed trait ItemSrc    extends ItemSrcDest
  sealed trait ItemDest   extends ItemSrcDest
  sealed trait Global     extends ItemSrcDest
  sealed trait IntraGroup extends ItemSrcDest
  sealed trait InterGroup extends ItemSrcDest

  final case class Extract( recipe: Recipe )          extends ItemSrc with IntraGroup with Global
  final case class Step( recipe: Recipe, group: Int ) extends ItemSrc with ItemDest with IntraGroup with Global
  final case class FromGroup( group: Int )            extends ItemSrc with IntraGroup with InterGroup
  final case class ToGroup( group: Int )              extends ItemDest with IntraGroup with InterGroup
  final case object Input                             extends ItemSrc with IntraGroup with InterGroup with Global
  final case object Byproduct                         extends ItemDest with IntraGroup with InterGroup with Global
  final case object Requested                         extends ItemDest with IntraGroup with InterGroup with Global

  // global: Extract | Step | Input | Byproduct | Requested
  // intra: *
  // inter: FromGroup | ToGroup | Input | Byproduct | Requested

  private def index( srcDest: ItemSrcDest ): ( Int, String, Option[Int] ) = srcDest match {
    case Input             => ( -2, "", None )
    case FromGroup( g )    => ( -2, "", Some( g ) )
    case Extract( recipe ) => ( -1, recipe.displayName, None )
    case Step( recipe, g ) => ( 0, recipe.displayName, Some( g ) )
    case ToGroup( g )      => ( 1, "", Some( g ) )
    case Requested         => ( 2, "", None )
    case Byproduct         => ( 3, "", None )
  }

  private def order[A <: ItemSrcDest]: Order[A] = Order.by( index )

  implicit val itemSrcDestShow: Show[ItemSrcDest]         = semiauto.show[ItemSrcDest]
  implicit val itemSrcDestOrder: Order[ItemSrcDest]       = order
  implicit val itemSrcDestOrdering: Ordering[ItemSrcDest] = Order.catsKernelOrderingForOrder

  object Global {
    implicit val globalItemSrcDestShow: Show[Global]         = semiauto.show[Global]
    implicit val globalItemSrcDestOrder: Order[Global]       = order
    implicit val globalItemSrcDestOrdering: Ordering[Global] = Order.catsKernelOrderingForOrder
  }
  object IntraGroup {
    implicit val intraGroupItemSrcDestShow: Show[IntraGroup]         = semiauto.show[IntraGroup]
    implicit val intraGroupItemSrcDestOrder: Order[IntraGroup]       = order
    implicit val intraGroupItemSrcDestOrdering: Ordering[IntraGroup] = Order.catsKernelOrderingForOrder
  }
  object InterGroup {
    implicit val interGroupItemSrcDestShow: Show[InterGroup]         = semiauto.show[InterGroup]
    implicit val interGroupItemSrcDestOrder: Order[InterGroup]       = order
    implicit val interGroupItemSrcDestOrdering: Ordering[InterGroup] = Order.catsKernelOrderingForOrder
  }
}
