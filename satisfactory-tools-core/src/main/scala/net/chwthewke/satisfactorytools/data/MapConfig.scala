package net.chwthewke.satisfactorytools
package data

import cats.syntax.apply._
import pureconfig.ConfigReader
import pureconfig.error.CannotConvert
import pureconfig.generic.semiauto
import scala.annotation.nowarn

import model.ClassName
import model.NativeClass
import model.ResourceDistrib

case class MapConfig( resourceNodes: Map[NativeClass, Map[ClassName, ResourceDistrib]] )

object MapConfig {
  private implicit val resourceDistribReader: ConfigReader[ResourceDistrib] =
    ConfigReader[Vector[Int]]
      .emap(
        counts =>
          ( counts.lift( 0 ), counts.lift( 1 ), counts.lift( 2 ) )
            .mapN( ResourceDistrib( _, _, _ ) )
            .toRight(
              CannotConvert( counts.mkString( "[", ", ", "]" ), "ResourceDistrib", "the array must have 3 elements" )
            )
      )

  implicit val mapConfigReader: ConfigReader[MapConfig] =
    semiauto.deriveReader[MapConfig]: @nowarn( "cat=lint-byname-implicit" )
}
