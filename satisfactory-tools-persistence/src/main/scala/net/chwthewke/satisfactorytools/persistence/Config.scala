package net.chwthewke.satisfactorytools
package persistence

import pureconfig.ConfigReader
import pureconfig.generic.semiauto

case class Config( databaseName: String, user: String, password: String )

object Config {
  implicit val configReader: ConfigReader[Config] = semiauto.deriveReader[Config]
}
