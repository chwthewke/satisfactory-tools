package net.chwthewke.satisfactorytools
package web.session

import cats.Applicative
import cats.Functor
import cats.effect.Ref
import cats.syntax.functor._
import fs2.Stream
import java.util.UUID

trait ProductionPlannerSession[F[_]] {
  def listFactories: Stream[F, ( String, StoredFactory )]

  def saveFactory( name: String, storedFactory: StoredFactory ): F[Unit]

  def getHistory: Stream[F, StoredFactory]

  def pushHistory( storedFactory: StoredFactory ): F[Unit]
}

object ProductionPlannerSession {
  case class InMemory[F[_]: Functor]( storage: Ref[F, Map[UUID, InMemorySession]], id: UUID )
      extends ProductionPlannerSession[F] {

    private def modifySession[A]( f: InMemorySession => ( InMemorySession, A ) ): F[A] =
      storage.modify { map =>
        val session       = map.getOrElse( id, InMemorySession.empty )
        val ( next, res ) = f( session )
        ( map + ( ( id, next ) ), res )
      }

    override def listFactories: Stream[F, ( String, StoredFactory )] =
      Stream.force(
        modifySession( sess => ( sess, sess.saved.toVector ) )
          .map( Stream.emits[F, ( String, StoredFactory )] )
      )

    override def saveFactory( name: String, storedFactory: StoredFactory ): F[Unit] =
      modifySession( sess => ( sess.copy( saved = sess.saved + ( ( name, storedFactory ) ) ), () ) )

    override def getHistory: Stream[F, StoredFactory] =
      Stream.force(
        modifySession( sess => ( sess, sess.history ) )
          .map( Stream.emits[F, StoredFactory] )
      )

    override def pushHistory( storedFactory: StoredFactory ): F[Unit] =
      modifySession( sess => ( sess.copy( history = storedFactory :: sess.history ), () ) )

  }

  case class Null[F[_]]()( implicit F: Applicative[F] ) extends ProductionPlannerSession[F] {
    override def listFactories: Stream[F, ( String, StoredFactory )] = Stream.empty

    override def saveFactory( name: String, storedFactory: StoredFactory ): F[Unit] = F.unit

    override def getHistory: Stream[F, StoredFactory] = Stream.empty

    override def pushHistory( storedFactory: StoredFactory ): F[Unit] = F.unit
  }

}
