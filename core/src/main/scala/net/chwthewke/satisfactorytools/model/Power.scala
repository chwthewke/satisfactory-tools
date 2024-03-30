package net.chwthewke.satisfactorytools
package model

import cats.Monoid
import cats.Show
import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

sealed abstract class Power {
  def average: Double

  def min: Double
  def max: Double

  def map( f: Double => Double ): Power

  def combine( other: Power ): Power
}

object Power {
  final case class Fixed( value: Double ) extends Power {
    override def average: Double = value

    override def min: Double = average
    override def max: Double = average

    override def map( f: Double => Double ): Power = Fixed( f( value ) )

    override def combine( other: Power ): Power = other match {
      case Fixed( otherValue )  => Fixed( value + otherValue )
      case Variable( min, max ) => Variable( value + min, value + max )
    }
  }

  final case class Variable( min: Double, max: Double ) extends Power {
    override def average: Double = ( min + max ) / 2d

    override def map( f: Double => Double ): Power = Variable( f( min ), f( max ) )

    override def combine( other: Power ): Variable = other match {
      case Fixed( value )                 => Variable( value + min, value + max )
      case Variable( otherMin, otherMax ) => Variable( min + otherMin, max + otherMax )
    }
  }

  implicit val powerMonoid: Monoid[Power] = new Monoid[Power] {
    override def empty: Power = Fixed( 0d )

    override def combine( x: Power, y: Power ): Power = x.combine( y )
  }

  implicit val powerShow: Show[Power] = Show.show {
    case Fixed( value )       => f"$value%6.2f"
    case Variable( min, max ) => f"$min%6.2f-$max%6.2f"
  }

  implicit val powerDecoder: Decoder[Power] = deriveDecoder[Power]
  implicit val powerEncoder: Encoder[Power] = deriveEncoder[Power]
}
