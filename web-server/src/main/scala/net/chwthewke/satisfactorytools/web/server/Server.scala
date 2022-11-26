package net.chwthewke.satisfactorytools
package web
package server

import cats.effect.Async
import cats.effect.Concurrent
import cats.effect.ExitCode
import cats.effect.Ref
import cats.effect.Resource
import cats.syntax.apply._
import cats.syntax.functor._
import cats.syntax.option._
import com.comcast.ip4s.Port
import com.comcast.ip4s.IpLiteralSyntax
import fs2.Stream
import fs2.concurrent.Signal
import fs2.concurrent.SignallingRef
import fs2.io.net.Network
import org.http4s.HttpApp
import org.http4s.blaze.server.BlazeServerBuilder
import pureconfig.ConfigSource
import pureconfig.module.catseffect.syntax._

import assets.IconIndex
import net.chwthewke.satisfactorytools.persistence.Resources

object Server {

  def initService[F[_]: Async]( jsFiles: Vector[String] ): Resource[F, ( Service[F], Port )] =
    for {
      iconIndex <- Resource.eval( ConfigSource.resources( "icons.conf" ).loadF[F, IconIndex]() )
      config    <- Resource.eval( ConfigSource.default.loadF[F, ServerConfig]() )
      db        <- Resources.managedTransactor[F]( config.db )
    } yield ( new Service[F]( jsFiles, iconIndex, config, DefsData.mapK( db.trans ) ), config.port )

  def run[F[_]: Async]( jsFiles: Vector[String] ): F[ExitCode] = {
    initService[F]( jsFiles )
      .use {
        case ( service, port ) =>
          mkStream[F]( port, service.route.orNotFound ).compile.lastOrError
      }
  }

  private def mkStream[F[_]: Async]( port: Port, app: HttpApp[F] ): Stream[F, ExitCode] =
    (
      Stream.eval( Ref[F].of( ExitCode.Success ) ),
      Stream.eval( mkSignal[F] )
    ).flatMapN( mkStreamWithSignal( port, app, _, _ ) )

  private def mkStreamWithSignal[F[_]: Async](
      port: Port,
      app: HttpApp[F],
      exitRef: Ref[F, ExitCode],
      stopSignal: SignallingRef[F, Boolean]
  ): Stream[F, ExitCode] =
    Stream.force( for {
      tcpPort <- Port
                  .fromInt( port.value + 1 )
                  .liftTo[F]( new RuntimeException( s"Invalid TCP shutdown port ${port.value + 1}" ) )
      tcpServer  = tcpShutdownServer( tcpPort, stopSignal )
      httpServer = httpAppServer[F]( port.value, app, stopSignal, exitRef )
    } yield tcpServer.merge( httpServer ) )

  private def tcpShutdownServer[F[_]: Async](
      port: Port,
      signalOut: SignallingRef[F, Boolean]
  ): Stream[F, Nothing] = {
    def stream( ownSignal: SignallingRef[F, Boolean] ): Stream[F, Nothing] =
      Network[F]
        .server( address = Some( ipv4"127.0.0.1" ), port = Some( port ) )
        .evalMap(
          _ =>
            signalOut.set( true ) // emit signal to stop
            //              *> sock.close   // TODO close incoming connection?
              *> ownSignal.set( true ) // stop the tcp server itself
        )
        .drain
        .interruptWhen( ownSignal )

    Stream.force( mkSignal[F].map( stream ) )
  }

  private def httpAppServer[F[_]: Async](
      port: Int,
      app: HttpApp[F],
      signal: Signal[F, Boolean],
      exitRef: Ref[F, ExitCode]
  ): Stream[F, ExitCode] =
    BlazeServerBuilder[F]
      .withHttpApp( app )
      .bindLocal( port )
      .serveWhile( signal, exitRef )

  private def mkSignal[F[_]: Concurrent]: F[SignallingRef[F, Boolean]] = SignallingRef[F, Boolean]( false )

}
