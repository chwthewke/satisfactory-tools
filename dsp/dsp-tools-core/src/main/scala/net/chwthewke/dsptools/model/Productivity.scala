package net.chwthewke.dsptools
package model

import cats.data.ZipVector
import cats.syntax.apply._
import spire.math.Rational

case class Productivity(
    speedIncreaseThousandths: Int,
    productIncreaseThousandths: Int,
    powerIncreaseThousandths: Int
) {
  def productIncrease: Rational = Rational( productIncreaseThousandths.toLong + 1000, 1000L )
  def powerIncrease: Double     = 1d + powerIncreaseThousandths / 1000d
}

object Productivity {

  def apply( level: Int ): Productivity = byLevel( level )

  val byLevel: Vector[Productivity] =
    (
      ZipVector( speedIncreaseThousandths ),
      ZipVector( productIncreaseThousandths ),
      ZipVector( powerIncreaseThousandths )
    ).mapN( Productivity( _, _, _ ) ).value

  private def speedIncreaseThousandths: Vector[Int] =
    Vector(0, 250, 500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2500 )

  private def productIncreaseThousandths: Vector[Int] =
    Vector(0, 125, 200, 225, 250, 275, 300, 325, 350, 375, 400 )

  private def powerIncreaseThousandths: Vector[Int] =
    Vector(0, 300, 700, 1100, 1500, 1900, 2300, 2700, 3100, 3500, 3900 )

}
