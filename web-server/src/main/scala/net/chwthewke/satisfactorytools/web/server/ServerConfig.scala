package net.chwthewke.satisfactorytools
package web
package server

import cats.syntax.either._
import com.comcast.ip4s.Port
import org.http4s.Uri.Scheme
import pureconfig.ConfigReader
import pureconfig.error.CannotConvert
import pureconfig.generic.semiauto

import persistence.{Config => DbConfig}

case class ServerConfig( scheme: Scheme, port: Port, db: DbConfig, cacheResources: Boolean )

object ServerConfig {
  private implicit val schemeConfigReader: ConfigReader[Scheme] =
    ConfigReader[String].emap(
      scheme => Scheme.fromString( scheme ).leftMap( pf => CannotConvert( scheme, "Scheme", pf.message ) )
    )

  private implicit val portConfigReader: ConfigReader[Port] =
    ConfigReader[Int].emap(
      port => Port.fromInt( port ).toRight( CannotConvert( port.toString, "Port", "Invalid port" ) )
    )

  implicit val serverConfigReader: ConfigReader[ServerConfig] = semiauto.deriveReader[ServerConfig]
}
