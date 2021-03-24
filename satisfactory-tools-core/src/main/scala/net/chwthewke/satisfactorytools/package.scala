package net.chwthewke

import cats.Show
import cats.syntax.show._
import com.flowtick.graphs.Identifiable
import pureconfig.ConfigReader

package object satisfactorytools {
  implicit def showIdentifiable[A: Show]: Identifiable[A] = Identifiable.identify( _.show )

  implicit val unitConfigReader: ConfigReader[Unit] =
    ConfigReader.fromCursor( _ => Right( () ) )

}
