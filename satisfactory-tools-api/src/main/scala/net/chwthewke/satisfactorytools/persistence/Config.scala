package net.chwthewke.satisfactorytools
package persistence

import pureconfig.ConfigReader
import pureconfig.generic.semiauto
import scala.annotation.nowarn

case class Config( databaseName: String, user: String, password: String )

object Config {
  implicit val configReader: ConfigReader[Config] = semiauto.deriveReader[Config]: @nowarn( "cat=lint-byname-implicit" )
}
