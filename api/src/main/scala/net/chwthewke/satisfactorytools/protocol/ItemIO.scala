package net.chwthewke.satisfactorytools
package protocol

import cats.Monoid
import cats.Show
import cats.derived.semiauto

import data.Countable

case class ItemIO(
    sources: Vector[Countable[Double, ItemSrcDest]],
    destinations: Vector[Countable[Double, ItemSrcDest]]
)

object ItemIO {
  implicit val itemIOShow: Show[ItemIO] = semiauto.show[ItemIO]
  implicit val itemIOMonoid: Monoid[ItemIO] = new Monoid[ItemIO] {
    override def empty: ItemIO =
      ItemIO( Vector.empty, Vector.empty )

    override def combine( x: ItemIO, y: ItemIO ): ItemIO =
      ItemIO(
        (x.sources ++ y.sources).gather,
        (x.destinations ++ y.destinations).gather
      )
  }

  def in( src: ItemSrcDest, amount: Double ): ItemIO =
    ItemIO( Vector( Countable( src, amount ) ), Vector.empty )

  def out( dest: ItemSrcDest, amount: Double ): ItemIO =
    ItemIO( Vector.empty, Vector( Countable( dest, amount ) ) )

}
