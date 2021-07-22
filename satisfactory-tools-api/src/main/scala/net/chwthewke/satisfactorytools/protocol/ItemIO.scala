package net.chwthewke.satisfactorytools
package protocol

import cats.Show
import cats.derived.semiauto

import data.Countable

case class ItemIO(
    sources: Vector[Countable[Double, ItemSrcDest]],
    destinations: Vector[Countable[Double, ItemSrcDest]]
)

object ItemIO {
  implicit val itemIOShow: Show[ItemIO] = semiauto.show[ItemIO]
}
