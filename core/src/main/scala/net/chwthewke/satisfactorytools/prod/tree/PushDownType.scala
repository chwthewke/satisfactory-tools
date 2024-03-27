package net.chwthewke.satisfactorytools
package prod
package tree

import cats.Show
import cats.derived.semiauto
import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

import data.ClassName

sealed trait PushDownType

object PushDownType {
  case object Full                    extends PushDownType // same as Fraction(1) for practicality
  case class Fraction( n: Int )       extends PushDownType
  case class Amount( amount: Double ) extends PushDownType
  case class For( recipe: ClassName ) extends PushDownType

  implicit val pushDownTypeDecoder: Decoder[PushDownType] = deriveDecoder[PushDownType]
  implicit val pushDownTypeEncoder: Encoder[PushDownType] = deriveEncoder[PushDownType]

  implicit val pushDownTypeShow: Show[PushDownType] = semiauto.show[PushDownType]
}
