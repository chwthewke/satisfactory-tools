package net.chwthewke.satisfactorytools
package persistence

import cats.data.OptionT
import cats.syntax.applicativeError._
import cats.syntax.functor._
import doobie._
import doobie.implicits._
import doobie.postgres.implicits._
import java.time.Instant
import java.util.UUID
import mouse.option._
import org.http4s.BasicCredentials
import org.http4s.Credentials
import scala.annotation.nowarn

import api.SessionApi
import protocol.Session
import protocol.SessionId
import protocol.UserId

object Sessions extends SessionApi[ConnectionIO] {

  override def newSession( credentials: Credentials ): ConnectionIO[Session] =
    BasicCredentials
      .unapply( credentials )
      .cata(
        up => initUserSession( UserName( up._1 ) ),
        FC.raiseError( new IllegalArgumentException( "Invalid credentials" ) )
      )

  @nowarn( "cat=lint-byname-implicit" )
  private def initUserSession( userName: UserName ): ConnectionIO[Session] =
    for {
      _         <- purgeExpiredSessions
      userId    <- statements.upsertUser.withUniqueGeneratedKeys[UserId]( "id" )( userName )
      sessionId <- FC.delay( UUID.randomUUID() ).map( SessionId( _ ) )
      expiry    <- statements.createSession( sessionId, userId ).unique
    } yield Session( sessionId, userId, expiry )

  private def purgeExpiredSessions: ConnectionIO[Unit] =
    statements.deleteOldSessions.run.attempt.void

  override def getSession( sessionId: SessionId ): OptionT[ConnectionIO, Session] =
    for {
      _      <- OptionT.liftF( purgeExpiredSessions )
      userId <- OptionT( statements.selectSessionUser( sessionId ).option )
      expiry <- OptionT.liftF( statements.refreshSession( sessionId ).unique )
    } yield Session( sessionId, userId, expiry )

  @nowarn( "cat=lint-byname-implicit" )
  object statements {
    val upsertUser: Update[UserName] =
      Update(
        // language=SQL
        """INSERT INTO "users" ( "name" )
          |VALUES ( ? )
          |ON CONFLICT ON CONSTRAINT "user_unique"
          |  DO UPDATE SET
          |    "name" = "excluded"."name"
          |""".stripMargin //
      )

    def selectSessionUser( sessionId: SessionId ): Query0[UserId] =
      // language=SQL
      sql"""SELECT
           |    "user_id"
           |FROM "sessions"
           |WHERE "id" = $sessionId
           |  AND "expiry" >= CURRENT_TIMESTAMP
           |""".stripMargin //
      .query

    def createSession( sessionId: SessionId, userId: UserId ): Query0[Instant] =
      // language=SQL
      sql"""INSERT INTO "sessions"
           |  ( "id", "user_id", "expiry" )
           |VALUES
           |  ( $sessionId, $userId, CURRENT_TIMESTAMP + INTERVAL '1 day' )
           |RETURNING
           |  "expiry"
           |""".stripMargin //
      .query

    def refreshSession( sessionId: SessionId ): Query0[Instant] =
      // language=SQL
      sql"""UPDATE "sessions"
           |SET
           |    "expiry" = CURRENT_TIMESTAMP + INTERVAL '1 day'
           |WHERE
           |    "id" = $sessionId
           |RETURNING
           |    "expiry"
           |""".stripMargin //
      .query

    val deleteOldSessions: Update0 =
      // language=SQL
      sql"""DELETE FROM "sessions"
           |WHERE "expiry" < CURRENT_TIMESTAMP + INTERVAL '10 minutes'
           |""".stripMargin //
      .update

  }
}
