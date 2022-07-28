package net.chwthewke.satisfactorytools
package model

import cats.kernel.Eq
import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

import data.ClassName
import data.Item

case class ResourceWeights( weights: Map[ClassName, Int] /* int coded btw `0` and `2 * range` inclusive */ ) {
  import ResourceWeights._

  def costs( resourceCaps: Map[Item, Double] ): Map[Item, Double] = {
    val raw = resourceCaps.map {
      case ( item, cap ) =>
        (
          item,
          1d / math.max( cap, 1e-5 ) *
            math.pow( 2d, (weights.getOrElse( item.className, range ) - range).toDouble / 4d )
        )
    }

    val sum = raw.values.sum

    raw.map { case ( item, rawCap ) => ( item, total * rawCap / sum ) }
  }

}

object ResourceWeights {
  val default: ResourceWeights = ResourceWeights( Map.empty )

  val total: Double = 1e6
  val range: Int    = 4 // weight between -range and range inclusive

  implicit val resourceWeightsEq: Eq[ResourceWeights] = Eq.by( _.weights )

  implicit val resourceWeightsDecoder: Decoder[ResourceWeights] = deriveDecoder[ResourceWeights]
  implicit val resourceWeightsEncoder: Encoder[ResourceWeights] = deriveEncoder[ResourceWeights]
}
