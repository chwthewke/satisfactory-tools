package net.chwthewke.satisfactorytools
package data

import cats.Show
import cats.derived.semiauto

case class IconData( dir: String, packageName: String, textureName: String )

object IconData {
  implicit val iconDataShow: Show[IconData] = semiauto.show[IconData]
}
