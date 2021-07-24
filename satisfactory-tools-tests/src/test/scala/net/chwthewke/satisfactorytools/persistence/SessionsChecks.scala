package net.chwthewke.satisfactorytools
package persistence

import java.util.UUID

import protocol.SessionId
import protocol.UserId

class SessionsChecks extends DatabaseSpec {
  val userId: UserId       = UserId( 1 )
  val sessionId: SessionId = SessionId( UUID.randomUUID() )

  "the statement" which {
    "creates or reads a user" must {
      "type check" in {
        check( Sessions.statements.upsertUser )
      }
    }

    "retrieves a user by session id" must {
      "type check" in {
        check( Sessions.statements.selectSessionUser( sessionId ) )
      }
    }

    "creates a session" must {
      "type check" in {
        check( Sessions.statements.createSession( sessionId, userId ) )
      }
    }

    "refreshes a session" must {
      "type check" in {
        check( Sessions.statements.refreshSession( sessionId ) )
      }
    }

    "deletes old sessions" must {
      "type check" in {
        check( Sessions.statements.deleteOldSessions )
      }
    }
  }

}
