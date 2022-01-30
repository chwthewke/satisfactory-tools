package net.chwthewke.dsptools
package model

import cats.Show
import cats.Order
import cats.syntax.show._

import gamedata.ItemProto

case class Item(
    displayName: String,
    proto: ItemProto
) {
  def id: Int                = proto.id
  def productive: Boolean    = proto.productive
  def productivityLevel: Int = proto.ability
  def productivityUses: Int  = proto.hpMax
}

object Item {
  implicit val itemShow: Show[Item]   = Show.show( item => show"${item.displayName} #${item.id}" )
  implicit val itemOrder: Order[Item] = Order.by( _.proto.id )
}
