package net.chwthewke.satisfactorytools
package model

import cats.Order
import cats.Show
import cats.derived.semiauto

case class ModelVersion( version: Int, name: String )

object ModelVersion {
  implicit val modelVersionShow: Show[ModelVersion]   = semiauto.show[ModelVersion]
  implicit val modelVersionOrder: Order[ModelVersion] = Order.by( _.version )
}
