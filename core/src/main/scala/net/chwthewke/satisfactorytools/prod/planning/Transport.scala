package net.chwthewke.satisfactorytools
package prod.planning

import cats.Show
import cats.Traverse
import cats.data.NonEmptyVector
import cats.derived.semiauto

/** previous invariant (without ProcessId indirection)
  *
  * `from.foldMap(_.produced(item.item)).sum == to.foldMap(_.consumed(item.item))`
  */
case class Transport[P]( amount: Double, from: NonEmptyVector[P], to: NonEmptyVector[P] )

object Transport {
  implicit def transportShow[P: Show]: Show[Transport[P]] = semiauto.show[Transport[P]]
  implicit val transportTraverse: Traverse[Transport]     = semiauto.traverse[Transport]
}
