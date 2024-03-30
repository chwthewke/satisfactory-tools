package net.chwthewke.satisfactorytools
package web.app

import cats.data.EitherT
import cats.data.Kleisli
import cats.data.OptionT
import cats.effect.Sync
import cats.syntax.show._
import java.util.UUID
import org.http4s.BasicCredentials
import org.http4s.Challenge
import org.http4s.ContextRequest
import org.http4s.HttpDate
import org.http4s.Request
import org.http4s.Response
import org.http4s.ResponseCookie
import org.http4s.SameSite
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.Authorization
import org.http4s.headers.Cookie
import org.http4s.headers.`Set-Cookie`
import org.http4s.headers.`WWW-Authenticate`
import org.http4s.server.ContextMiddleware
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import scala.util.Try

import api.SessionApi
import protocol.Session
import protocol.SessionId
import protocol.UserName

case class SessionMiddleware[F[_]: Sync]( sessions: SessionApi[F] ) extends ContextMiddleware[F, Session] {

  val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName( "APP" )

  private val dsl = new Http4sDsl[F] {}
  import dsl._

  val unauthorized: F[Response[F]] =
    Unauthorized( `WWW-Authenticate`( Challenge( "Basic", "satisfactory tools", Map( "charset" -> "UTF-8" ) ) ) )

  def getSession( request: Request[F] ): EitherT[F, Response[F], Session] =
    OptionT
      .fromOption[F]( getSessionId( request ) )
      .semiflatTap( id => logger.debug( show"Got session $id from request" ) )
      .flatMap( sessions.getSession )
      .semiflatTap( session => logger.debug( show"Got session $session from db" ) )
      .orElse(
        newSessionFromCredentials( request )
          .semiflatTap( session => logger.debug( show"New session $session" ) )
      )
      .toRightF( unauthorized )

  private def getSessionId( request: Request[F] ): Option[SessionId] =
    request.headers
      .get[Cookie]
      .flatMap( _.values.find( _.name == Application.cookieSessionIdKey ) )
      .flatMap( c => Try( UUID.fromString( c.content ) ).toOption )
      .map( SessionId( _ ) )

  private def newSessionFromCredentials( request: Request[F] ): OptionT[F, Session] =
    OptionT
      .fromOption(
        request.headers
          .get[Authorization]
          .map( _.credentials )
          .flatMap( BasicCredentials.unapply )
          .map( bc => UserName( bc._1 ) )
      )
      .semiflatMap( sessions.newSession )

  private def setCookie( session: Session ): `Set-Cookie` =
    `Set-Cookie`(
      ResponseCookie(
        SessionMiddleware.cookieSessionIdKey,
        session.id.show,
        expires = Some( HttpDate.fromInstant( session.expiry ).getOrElse( HttpDate.MaxValue ) ),
        sameSite = Some( SameSite.Strict ),
        httpOnly = true,
        secure = true
      )
    )

  override def apply(
      svc: Kleisli[OptionT[F, *], ContextRequest[F, Session], Response[F]]
  ): Kleisli[OptionT[F, *], Request[F], Response[F]] =
    Kleisli( req =>
      OptionT.liftF( getSession( req ).value ).flatMap {
        case Left( resp ) => OptionT.pure[F]( resp )
        case Right( session ) =>
          svc.run( ContextRequest( session, req ) ).map( _.putHeaders( setCookie( session ) ) )
      }
    )

}

object SessionMiddleware {
  val cookieSessionIdKey = "session-id"
}
