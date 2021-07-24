package net.chwthewke.satisfactorytools
package api

import cats.data.OptionT
import cats.~>
import org.http4s.Credentials

import protocol.Session
import protocol.SessionId

trait SessionApi[F[_]] { self =>
  def newSession( credentials: Credentials ): F[Session]

  def getSession( sessionId: SessionId ): OptionT[F, Session]

  def mapK[G[_]]( f: F ~> G ): SessionApi[G] = new SessionApi[G] {
    override def newSession( credentials: Credentials ): G[Session] =
      f( self.newSession( credentials ) )

    override def getSession( sessionId: SessionId ): OptionT[G, Session] =
      self.getSession( sessionId ).mapK( f )
  }
}
