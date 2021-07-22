package net.chwthewke.satisfactorytools
package api

import cats.data.OptionT
import cats.~>
import org.http4s.Credentials

import protocol.SessionId
import protocol.UserId

trait SessionApi[F[_]] { self =>
  def newSession( credentials: Credentials ): F[( UserId, SessionId )]

  def getUser( sessionId: SessionId ): OptionT[F, UserId]

  def mapK[G[_]]( f: F ~> G ): SessionApi[G] = new SessionApi[G] {
    override def newSession( credentials: Credentials ): G[( UserId, SessionId )] =
      f( self.newSession( credentials ) )

    override def getUser( sessionId: SessionId ): OptionT[G, UserId] =
      self.getUser( sessionId ).mapK( f )
  }
}
