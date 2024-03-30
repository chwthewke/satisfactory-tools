package net.chwthewke.satisfactorytools
package protocol

import cats.Monoid
import cats.Show
import cats.derived.semiauto

import data.Countable

case class ItemIO[+A <: ItemSrcDest](
    sources: Vector[Countable[Double, A]],
    destinations: Vector[Countable[Double, A]]
)

object ItemIO {
  implicit def itemIOShow[A <: ItemSrcDest: Show]: Show[ItemIO[A]] = semiauto.show[ItemIO[A]]
  implicit def itemIOMonoid[A <: ItemSrcDest]: Monoid[ItemIO[A]] = new Monoid[ItemIO[A]] {
    override def empty: ItemIO[A] =
      ItemIO( Vector.empty, Vector.empty )

    override def combine( x: ItemIO[A], y: ItemIO[A] ): ItemIO[A] =
      ItemIO(
        ( x.sources ++ y.sources ).gather,
        ( x.destinations ++ y.destinations ).gather
      )
  }

  def in[A <: ItemSrcDest]( src: A, amount: Double ): ItemIO[A] =
    ItemIO( Vector( Countable( src, amount ) ), Vector.empty )

  def out[A <: ItemSrcDest]( dest: A, amount: Double ): ItemIO[A] =
    ItemIO( Vector.empty, Vector( Countable( dest, amount ) ) )
}
