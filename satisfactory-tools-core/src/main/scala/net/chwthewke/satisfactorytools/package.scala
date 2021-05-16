package net.chwthewke

import pureconfig.ConfigReader

package object satisfactorytools {

  implicit val unitConfigReader: ConfigReader[Unit] =
    ConfigReader.fromCursor( _ => Right( () ) )

}
