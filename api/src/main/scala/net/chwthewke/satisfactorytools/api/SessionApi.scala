package net.chwthewke.satisfactorytools
package api

import cats.data.OptionT
import cats.~>

import protocol.Session
import protocol.SessionId
import protocol.UserName

trait SessionApi[F[_]] { self =>
  def newSession( userName: UserName ): F[Session]

  def getSession( sessionId: SessionId ): OptionT[F, Session]

  def mapK[G[_]]( f: F ~> G ): SessionApi[G] = new SessionApi[G] {
    override def newSession( userName: UserName ): G[Session] =
      f( self.newSession( userName ) )

    override def getSession( sessionId: SessionId ): OptionT[G, Session] =
      self.getSession( sessionId ).mapK( f )
  }
}
