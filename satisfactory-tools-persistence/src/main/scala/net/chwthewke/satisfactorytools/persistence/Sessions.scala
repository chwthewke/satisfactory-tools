package net.chwthewke.satisfactorytools
package persistence

import cats.data.OptionT
import cats.syntax.applicativeError._
import cats.syntax.functor._
import doobie._
import doobie.implicits._
import doobie.postgres.implicits._
import doobie.util.invariant.UnexpectedEnd
import java.time.Instant
import java.util.UUID

import api.SessionApi
import protocol.Session
import protocol.SessionId
import protocol.UserId
import protocol.UserName

object Sessions extends SessionApi[ConnectionIO] {

  override def newSession( userName: UserName ): ConnectionIO[Session] =
    initUserSession( userName )

  private def initUserSession( userName: UserName ): ConnectionIO[Session] =
    for {
      _ <- purgeExpiredSessions
      userId <- statements.upsertUser
                 .withUniqueGeneratedKeys[UserId]( "id" )( userName )
                 .adaptErr {
                   case UnexpectedEnd => Error( s"Unable to create user $userName" )
                 }
      sessionId <- FC.delay( UUID.randomUUID() ).map( SessionId( _ ) )
      expiry <- statements
                 .createSession( sessionId, userId )
                 .unique
                 .adaptErr {
                   case UnexpectedEnd => Error( s"Could not create session $sessionId for user $userId" )
                 }
    } yield Session( sessionId, userId, userName, expiry )

  private def purgeExpiredSessions: ConnectionIO[Unit] =
    statements.deleteOldSessions.run.attempt.void

  override def getSession( sessionId: SessionId ): OptionT[ConnectionIO, Session] =
    for {
      _                    <- OptionT.liftF( purgeExpiredSessions )
      ( userId, userName ) <- OptionT( statements.selectSessionUser( sessionId ).option )
      expiry <- OptionT.liftF( statements.refreshSession( sessionId ).unique.adaptErr {
                 case UnexpectedEnd => Error( s"Could not refresh session $sessionId" )
               } )
    } yield Session( sessionId, userId, userName, expiry )

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

    def selectSessionUser( sessionId: SessionId ): Query0[( UserId, UserName )] =
      // language=SQL
      sql"""SELECT
           |    s."user_id"
           |  , u."name"
           |FROM "sessions" s
           |INNER JOIN "users" u on u."id" = s."user_id"                
           |WHERE s."id" = $sessionId
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
