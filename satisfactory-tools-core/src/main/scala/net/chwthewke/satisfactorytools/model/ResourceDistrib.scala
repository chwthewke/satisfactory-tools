package net.chwthewke.satisfactorytools
package model

import cats.Show
import cats.syntax.foldable._
import cats.syntax.show._

case class ResourceDistrib( impureNodes: Int, normalNodes: Int, pureNodes: Int ) {
  def value( extractorRecipe: Recipe[Machine, Item], clockSpeed: Options.ClockSpeed, belt: Options.Belt ): Double =
    Vector( ( impureNodes, 0.5d ), ( normalNodes, 1.0d ), ( pureNodes, 2.0d ) )
      .foldMap {
        case ( count, modifier ) =>
          count *
            (extractorRecipe.productsPerMinute.head.amount * clockSpeed.percent / 100d * modifier)
              .min( belt.itemsPerMinute.toDouble )
      }

  override def toString: String = show"Impure: $impureNodes, Normal: $normalNodes, Pure: $pureNodes"
}

object ResourceDistrib {
  implicit val resourceDistribShow: Show[ResourceDistrib] = Show.fromToString[ResourceDistrib]
}
