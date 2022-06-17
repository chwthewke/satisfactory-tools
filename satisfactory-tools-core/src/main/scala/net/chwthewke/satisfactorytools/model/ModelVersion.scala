package net.chwthewke.satisfactorytools
package model

import cats.Show
import cats.derived.semiauto

case class ModelVersion( version: Int, name: String )

object ModelVersion {
  implicit val modelVersionShow: Show[ModelVersion] = semiauto.show[ModelVersion]
}
