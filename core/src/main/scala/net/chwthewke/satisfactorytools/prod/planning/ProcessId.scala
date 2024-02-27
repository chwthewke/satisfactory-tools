package net.chwthewke.satisfactorytools
package prod
package planning

import cats.Order
import cats.Show
import cats.syntax.show._

import data.ClassName
import data.Countable
import data.Item
import model.Recipe

sealed abstract class ProcessId extends Product

object ProcessId {
  def recipe( r: ClockedRecipe ): ProcessId.Natural =
    OfRecipe( r.recipe.item.className )

  def recipe[N]( r: Countable[N, Recipe] ): ProcessId.Natural =
    OfRecipe( r.item.className )

  def request[N]( i: Countable[N, Item] ): ProcessId.Natural =
    OfRequest( i.item.className )

  def byProduct[N]( i: Countable[N, Item] ): ProcessId.Natural =
    OfByProduct( i.item.className )

  sealed abstract class Natural extends ProcessId
  object Natural {
    implicit val naturalProcessIdOrder: Order[ProcessId.Natural]       = Order.by( processIdToOrdered )
    implicit val naturalProcessIdOrdering: Ordering[ProcessId.Natural] = Order.catsKernelOrderingForOrder
  }

  case class OfRecipe( name: ClassName ) extends Natural {
    override def toString: String = show"[$name]"
  }
  case class OfRequest( item: ClassName ) extends Natural {
    override def toString: String = show"R[$item]"
  }
  case class OfByProduct( item: ClassName ) extends Natural {
    override def toString: String = show"X[$item]"
  }
  case class Synthetic( id: Int ) extends ProcessId {
    override def toString: String = show"$id"
  }
  object Synthetic {
    implicit val syntheticOrder: Order[Synthetic]       = Order.by( _.id )
    implicit val syntheticOrdering: Ordering[Synthetic] = Order.catsKernelOrderingForOrder
  }

  implicit val processIdShow: Show[ProcessId]         = Show.fromToString
  implicit val processIdOrder: Order[ProcessId]       = Order.by( processIdToOrdered )
  implicit val processIdOrdering: Ordering[ProcessId] = Order.catsKernelOrderingForOrder

  private def processIdToOrdered: ProcessId => ( Int, String ) = {
    case OfRecipe( name )    => ( -3, name.show )
    case OfRequest( item )   => ( -2, item.show )
    case OfByProduct( item ) => ( -1, item.show )
    case Synthetic( id )     => ( id, "" )
  }

}
