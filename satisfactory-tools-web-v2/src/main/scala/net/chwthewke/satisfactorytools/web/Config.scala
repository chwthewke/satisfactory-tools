package net.chwthewke.satisfactorytools
package web

import pureconfig.ConfigReader
import pureconfig.generic.semiauto

import persistence.{Config => DbConfig}

case class Config( db: DbConfig )

object Config {
  implicit val configReader: ConfigReader[Config] = semiauto.deriveReader[Config]
}
