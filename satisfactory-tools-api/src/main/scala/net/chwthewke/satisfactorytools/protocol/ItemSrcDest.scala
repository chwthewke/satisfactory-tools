package net.chwthewke.satisfactorytools
package protocol

import cats.Eq
import cats.Show
import cats.derived.semiauto

sealed trait ItemSrcDest extends Product

object ItemSrcDest {
  final case class Recipe( name: String ) extends ItemSrcDest
  final case object Input                 extends ItemSrcDest
  final case object Output                extends ItemSrcDest
  final case object Byproduct             extends ItemSrcDest
  final case object Requested             extends ItemSrcDest

  implicit val itemSrcDestShow: Show[ItemSrcDest] = semiauto.show[ItemSrcDest]
  implicit val itemSrcDestEq: Eq[ItemSrcDest]     = semiauto.eq[ItemSrcDest]
}
