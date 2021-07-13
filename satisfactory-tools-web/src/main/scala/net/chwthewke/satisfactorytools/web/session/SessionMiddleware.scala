package net.chwthewke.satisfactorytools
package web.session

import cats.data.Kleisli
import cats.data.OptionT
import cats.effect.kernel.Ref
import cats.effect.kernel.Sync
import cats.syntax.apply._
import java.util.UUID
import org.http4s.HttpDate
import org.http4s.Request
import org.http4s.Response
import org.http4s.ResponseCookie
import org.http4s.SameSite
import org.http4s.headers.Cookie
import org.http4s.headers.`Set-Cookie`
import org.http4s.server.HttpMiddleware
import org.typelevel.vault.Key
import scala.util.Try

case class SessionMiddleware[F[_]](
    sessionKey: Key[ProductionPlannerSession[F]],
    storage: Ref[F, Map[UUID, InMemorySession]]
)( implicit F: Sync[F] )
    extends HttpMiddleware[F] {
  override def apply(
      service: Kleisli[OptionT[F, *], Request[F], Response[F]]
  ): Kleisli[OptionT[F, *], Request[F], Response[F]] =
    Kleisli(
      req =>
        for {
          sessionId <- OptionT.liftF( getSessionId( req ) )
          resp <- service.run(
                   req.withAttribute( sessionKey, ProductionPlannerSession.InMemory( storage, sessionId ) )
                 )
        } yield resp.putHeaders( setCookie( sessionId ) )
    )

  private def getSessionId( request: Request[F] ): F[UUID] =
    request.headers
      .get[Cookie]
      .flatMap( _.values.find( _.name == SessionMiddleware.cookieSessionIdKey ) )
      .flatMap( c => Try( UUID.fromString( c.name ) ).toOption )
      .fold( F.delay( UUID.randomUUID() ) )( F.pure )

  private def setCookie( sessionId: UUID ): `Set-Cookie` =
    `Set-Cookie`(
      ResponseCookie(
        SessionMiddleware.cookieSessionIdKey,
        sessionId.toString,
        expires = Some( HttpDate.MaxValue ),
        sameSite = Some( SameSite.Strict ),
        httpOnly = true,
        secure = true
      )
    )

}

object SessionMiddleware {
  val cookieSessionIdKey = "id"

  def init[F[_]: Sync]: F[SessionMiddleware[F]] =
    (
      Key.newKey[F, ProductionPlannerSession[F]],
      Ref[F].of( Map.empty[UUID, InMemorySession] )
    ).mapN( SessionMiddleware[F]( _, _ ) )
}
