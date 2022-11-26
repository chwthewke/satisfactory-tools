package net.chwthewke.satisfactorytools
package web.front

import cats.syntax.either._
import org.http4s.Uri
import scala.scalajs.js.annotation.JSGlobalScope
import scala.scalajs.js.annotation.JSName
import scalajs.js

case class Config( baseUri: Uri )

object Config {

  def load: Either[Throwable, Config] =
    for {
      uriText <- Either.catchNonFatal( Globals.serverUri )
      base    <- Uri.fromString( uriText )
    } yield Config( base )

  @js.native
  @JSGlobalScope
  private object Globals extends js.Object {
    @JSName( "serverUri" )
    def serverUri: String = js.native
  }
}
