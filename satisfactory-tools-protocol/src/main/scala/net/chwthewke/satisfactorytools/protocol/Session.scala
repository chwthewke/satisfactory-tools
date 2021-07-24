package net.chwthewke.satisfactorytools
package protocol

import cats.Show
import cats.derived.semiauto
import io.chrisdavenport.cats.time.instances.instant._
import java.time.Instant

case class Session( id: SessionId, userId: UserId, expiry: Instant )

object Session {
  implicit val sessionShow: Show[Session] = semiauto.show[Session]
}
