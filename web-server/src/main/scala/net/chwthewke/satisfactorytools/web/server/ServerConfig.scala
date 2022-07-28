package net.chwthewke.satisfactorytools
package web
package server

import com.comcast.ip4s.Port
import pureconfig.ConfigReader
import pureconfig.error.CannotConvert
import pureconfig.generic.semiauto

import persistence.{Config => DbConfig}

case class ServerConfig( port: Port, db: DbConfig )

object ServerConfig {
  private implicit val portConfigReader: ConfigReader[Port] =
    ConfigReader[Int].emap(
      port => Port.fromInt( port ).toRight( CannotConvert( port.toString, "Port", "Invalid port" ) )
    )

  implicit val serverConfigReader: ConfigReader[ServerConfig] = semiauto.deriveReader[ServerConfig]
}
