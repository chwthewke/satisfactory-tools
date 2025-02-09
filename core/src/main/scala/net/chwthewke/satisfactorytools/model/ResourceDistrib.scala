package net.chwthewke.satisfactorytools
package model

import algebra.lattice.MeetSemilattice
import cats.Eq
import cats.Monoid
import cats.Show
import cats.derived.semiauto
import cats.syntax.all._
import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

case class ResourceDistrib( impureNodes: Int, normalNodes: Int, pureNodes: Int ) {
  def value( extractorRecipe: Recipe, clockSpeed: Options.ClockSpeed, belt: Options.Belt ): Double =
    Vector( ( impureNodes, 0.5d ), ( normalNodes, 1.0d ), ( pureNodes, 2.0d ) )
      .foldMap {
        case ( count, modifier ) =>
          count *
            ( extractorRecipe.productsPerMinute.head.amount * clockSpeed.percent / 100d * modifier )
              .min( belt.itemsPerMinute.toDouble )
      }

  def get( purity: ResourcePurity ): Int = purity match {
    case ResourcePurity.Impure => impureNodes
    case ResourcePurity.Normal => normalNodes
    case ResourcePurity.Pure   => pureNodes
  }

  override def toString: String = show"Impure: $impureNodes, Normal: $normalNodes, Pure: $pureNodes"
}

object ResourceDistrib {
  def of( purity: ResourcePurity, amount: Int ): ResourceDistrib =
    purity match {
      case ResourcePurity.Pure   => ResourceDistrib( 0, 0, amount )
      case ResourcePurity.Normal => ResourceDistrib( 0, amount, 0 )
      case ResourcePurity.Impure => ResourceDistrib( amount, 0, 0 )
    }

  implicit val resourceDistribMonoid: Monoid[ResourceDistrib] = semiauto.monoid[ResourceDistrib]

  implicit val resourceDistribShow: Show[ResourceDistrib] = Show.fromToString[ResourceDistrib]

  implicit val resourceDistribMeetSemilattice: MeetSemilattice[ResourceDistrib] = new MeetSemilattice[ResourceDistrib] {
    override def meet( lhs: ResourceDistrib, rhs: ResourceDistrib ): ResourceDistrib =
      ResourceDistrib(
        lhs.impureNodes.min( rhs.impureNodes ),
        lhs.normalNodes.min( rhs.normalNodes ),
        lhs.pureNodes.min( rhs.pureNodes )
      )
  }

  implicit val resourceDistribEq: Eq[ResourceDistrib] = semiauto.eq[ResourceDistrib]

  implicit val resourceDistribDecoder: Decoder[ResourceDistrib] = deriveDecoder[ResourceDistrib]
  implicit val resourceDistribEncoder: Encoder[ResourceDistrib] = deriveEncoder[ResourceDistrib]
}
