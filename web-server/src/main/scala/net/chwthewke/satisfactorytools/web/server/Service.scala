package net.chwthewke.satisfactorytools
package web
package server

import cats.data.OptionT
import cats.effect.Sync
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.option._
import cats.syntax.traverse._
import cats.syntax.semigroupk._
import com.comcast.ip4s.Host
import com.comcast.ip4s.Port
import org.http4s.Headers
import org.http4s.HttpRoutes
import org.http4s.Request
import org.http4s.Response
import org.http4s.StaticFile
import org.http4s.Uri
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.dsl.Http4sDsl
import scalatags.Text.all._

import assets.IconIndex
import protocol.ModelVersionId
import web.api.DefsApi

class Service[F[_]: Sync](
    jsFiles: Vector[String],
    iconIndex: IconIndex,
    serverConfig: ServerConfig,
    defs: DefsApi[F]
) {

  private val dsl = new Http4sDsl[F] {}
  import dsl._

  private val pages: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ GET -> Root =>
      apiUri( req.headers )
        .fold(
          BadRequest( _ ),
          serverUri => Ok( indexHtml( serverUri ) )
        )
  }

  private def apiUri( headers: Headers ): Either[String, Uri] =
    headers
      .get[org.http4s.headers.Host]
      .toRight( "Missing Host header" )
      .flatMap { host =>
        (
          Host.fromString( host.host ).toValidNel( s"invalid host ${host.host}" ),
          host.port.traverse( Port.fromInt ).toValidNel( s"invalid port ${host.port}" )
        ).mapN(
            ( h, p ) =>
              Uri(
                scheme = Some( serverConfig.scheme ),
                authority = Some( Uri.Authority( host = Uri.Host.fromIp4sHost( h ), port = p.map( _.value ) ) )
              )
          )
          .toEither
          .leftMap( _.mkString_( "Invalid Host header: ", ", ", "" ) )
      }

  private def getStaticFile( resource: String, request: Request[F] ): OptionT[F, Response[F]] =
    StaticFile.fromResource( resource, req = Option.when( serverConfig.cacheResources )( request ) )

  private val resourcesRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ GET -> Root / "icon" / IntVar( version ) / textureName if textureName.endsWith( ".png" ) =>
      icon( version, textureName.stripSuffix( ".png" ), req ).getOrElseF( NotFound() )
    case req @ GET -> Service.StaticResource( resource ) =>
      getStaticFile( resource, req ).getOrElseF( NotFound() )
  }

  private val apiRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "api" / "models" => Ok( defs.getVersions )
    case GET -> Root / "api" / "model" / IntVar( modelVersion ) =>
      defs.getModel( ModelVersionId( modelVersion ) ).foldF( NotFound() )( Ok( _ ) )
  }

  private def icon( version: Int, textureName: String, req: Request[F] ): OptionT[F, Response[F]] =
    OptionT
      .fromOption[F]( iconIndex.getIconPath( version, textureName ) )
      .map( p => s"img/$p" )
      .flatMap( getStaticFile( _, req ) )

  val route: HttpRoutes[F] = pages <+> apiRoute <+> resourcesRoute

  private def indexHtml( serverUri: Uri ): Tag =
    html(
      lang := "en",
      head(
        meta( charset := "UTF-8" ),
        meta( name := "viewport", content := "width=device-width, initial-scale=1" ),
        link( rel := "stylesheet", href := "/res/css/bulma.min.css" ),
        script(
          `type` := "text/javascript",
          raw( s"""const serverUri = "${serverUri.renderString}";
                  |""".stripMargin )
        )
      ),
      body(
        div( id := "app" ),
        jsFiles.filter( _.endsWith( ".js" ) ).map( f => script( src := s"res/$f" ) )
      )
    )

}

object Service {

  val staticExtensions: Set[String] = Set(
    ".css",
    ".css.map",
    ".js",
    ".js.map",
    ".png"
  )

  object StaticResource {
    def unapply( path: Uri.Path ): Option[String] =
      for {
        _ <- path.segments.headOption.filter( _.decoded() == "res" )
        rest = path.segments.tail.map( _.decoded() )
        fileName <- rest.lastOption if staticExtensions.exists( fileName.endsWith )
      } yield rest.mkString( "/" )

  }
}
