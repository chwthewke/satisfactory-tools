package net.chwthewke.satisfactorytools
package protocol

import cats.Show
import cats.derived.semiauto
import java.time.Instant
import org.typelevel.cats.time.instances.instant._

case class Session( id: SessionId, userId: UserId, userName: UserName, expiry: Instant )

object Session {
  implicit val sessionShow: Show[Session] = semiauto.show[Session]
}
