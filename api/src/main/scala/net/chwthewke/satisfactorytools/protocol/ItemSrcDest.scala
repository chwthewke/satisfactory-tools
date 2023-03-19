package net.chwthewke.satisfactorytools
package protocol

import cats.Order
import cats.Show
import cats.derived.semiauto

import model.Recipe

sealed trait ItemSrcDest extends Product

object ItemSrcDest {
  sealed trait Global     extends ItemSrcDest
  sealed trait IntraGroup extends ItemSrcDest
  sealed trait InterGroup extends ItemSrcDest

  final case class Step( recipe: Recipe, group: Option[Int] ) extends IntraGroup with Global
  final case class FromGroup( group: Int )                    extends IntraGroup with InterGroup
  final case class ToGroup( group: Int )                      extends IntraGroup with InterGroup
  final case object Input                                     extends IntraGroup with InterGroup with Global
  final case object Output                                    extends IntraGroup
  final case object Byproduct                                 extends IntraGroup with InterGroup with Global
  final case object Requested                                 extends IntraGroup with InterGroup with Global

  // global: Step | Input | Byproduct | Requested
  // intra: *
  // inter: FromGroup | ToGroup | Input | ByProduct | Requested

  private def index( srcDest: ItemSrcDest ): ( Int, String, Option[Int] ) = srcDest match {
    case Input             => ( -1, "", None )
    case FromGroup( g )    => ( -1, "", Some( g ) )
    case Step( recipe, g ) => ( 0, recipe.displayName, g )
    case Output            => ( 1, "", None )
    case ToGroup( g )      => ( 1, "", Some( g ) )
    case Requested         => ( 2, "", None )
    case Byproduct         => ( 3, "", None )
  }

  private def order[A <: ItemSrcDest]: Order[A] = Order.by( index )

  implicit val itemSrcDestShow: Show[ItemSrcDest]   = semiauto.show[ItemSrcDest]
  implicit val itemSrcDestOrder: Order[ItemSrcDest] = order

  object Global {
    implicit val globalItemSrcDestShow: Show[Global]   = semiauto.show[Global]
    implicit val globalItemSrcDestOrder: Order[Global] = order
  }
  object IntraGroup {
    implicit val intraGroupItemSrcDestShow: Show[IntraGroup]   = semiauto.show[IntraGroup]
    implicit val intraGroupItemSrcDestOrder: Order[IntraGroup] = order
  }
  object InterGroup {
    implicit val interGroupItemSrcDestShow: Show[InterGroup]   = semiauto.show[InterGroup]
    implicit val interGroupItemSrcDestOrder: Order[InterGroup] = order
  }
}
