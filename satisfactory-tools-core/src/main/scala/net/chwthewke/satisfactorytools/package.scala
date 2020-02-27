package net.chwthewke

import cats.Show
import cats.syntax.show._
import com.flowtick.graphs.Identifiable

package object satisfactorytools {
  implicit def showIdentifiable[A: Show]: Identifiable[A] = Identifiable.identify( _.show )

}
