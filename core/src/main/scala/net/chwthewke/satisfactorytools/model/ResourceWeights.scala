package net.chwthewke.satisfactorytools
package model

import cats.kernel.Eq

import data.Item

case class ResourceWeights( weights: Map[Item, Int] /* int coded btw `0` and `2 * range` inclusive */ ) {
  import ResourceWeights._

  def costs( resourceCaps: Map[Item, Double] ): Map[Item, Double] = {
    val raw = resourceCaps.map {
      case ( item, cap ) =>
        (
          item,
          1d / math.max( cap, 1e-5 ) * math.pow( 2d, (weights.getOrElse( item, range ) - range).toDouble / 4d )
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

  implicit val resourceWeightsEq: Eq[ResourceWeights] =
    Eq.by( _.weights.filterNot { case ( _, w ) => w == range } )
}
