package net.chwthewke.satisfactorytools
package web
package server

import cats.data.OptionT
import cats.effect.Sync
import cats.syntax.apply._
import cats.syntax.semigroupk._
import org.http4s.HttpRoutes
import org.http4s.Request
import org.http4s.Response
import org.http4s.StaticFile
import org.http4s.Uri
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.dsl.Http4sDsl
import pureconfig.ConfigSource
import pureconfig.module.catseffect.syntax._
import scalatags.Text.all._
import assets.IconIndex
import cats.effect.Async
import cats.effect.kernel.Resource
import protocol.ModelVersionId
import web.api.DefsApi

class Service[F[_]: Sync]( jsFiles: Vector[String], iconIndex: IconIndex, defs: DefsApi[F] ) {

  private val dsl = new Http4sDsl[F] {}
  import dsl._

  private val pages: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root =>
      Ok( indexHtml )
  }

  private val resourcesRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ GET -> Root / "icon" / IntVar( version ) / textureName if textureName.endsWith( ".png" ) =>
      icon( version, textureName.stripSuffix( ".png" ), req ).getOrElseF( NotFound() )
    case req @ GET -> Service.StaticResource( resource ) =>
      StaticFile.fromResource( resource, req = Some( req ) ).getOrElseF( NotFound() )
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
      .flatMap( StaticFile.fromResource( _, req = Some( req ) ) )

  val route: HttpRoutes[F] = pages <+> apiRoute <+> resourcesRoute

  private def indexHtml: Tag =
    html(
      lang := "en",
      head(
        meta( charset := "UTF-8" ),
        meta( name := "viewport", content := "width=device-width, initial-scale=1" ),
        link( rel := "stylesheet", href := "/res/css/bulma.min.css" )
      ),
      body(
        div( id := "app" ),
        jsFiles.filter( _.endsWith( ".js" ) ).map( f => script( src := s"res/$f" ) )
      )
    )

}

object Service {
  def apply[F[_]: Async]( jsFiles: Vector[String], dbConfig: persistence.Config ): Resource[F, Service[F]] =
    (
      Resource.eval(
        ConfigSource
          .resources( "icons.conf" )
          .loadF[F, IconIndex]()
      ),
      persistence.Resources.managedTransactor[F]( dbConfig )
    ).mapN(
      ( iconIndex, xa ) => new Service( jsFiles, iconIndex, DefsData.mapK( xa.trans ) )
    )

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
