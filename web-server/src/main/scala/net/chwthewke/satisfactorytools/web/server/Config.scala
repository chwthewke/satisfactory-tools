package net.chwthewke.satisfactorytools
package web
package server

import pureconfig.ConfigReader
import pureconfig.generic.semiauto

case class Config( db: persistence.Config, port: Int )

object Config {
  implicit val configReader: ConfigReader[Config] =
    semiauto.deriveReader[Config]
}
