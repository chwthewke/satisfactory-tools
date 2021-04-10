package net.chwthewke.satisfactorytools
package data

import pureconfig.ConfigReader
import pureconfig.generic.semiauto
import scala.annotation.nowarn

import model.ClassName
import model.Countable

final case class ProductionConfig(
    items: Vector[Countable[ClassName, Double]],
    recipes: Vector[ClassName]
)

object ProductionConfig {

  implicit val productionConfigReader: ConfigReader[ProductionConfig] = {

    implicit val classNameReader: ConfigReader[ClassName] = ConfigReader[String].map( ClassName( _ ) )
    implicit val itemsReader: ConfigReader[Vector[Countable[ClassName, Double]]] =
      ConfigReader[Map[ClassName, Double]]
        .map( _.map( (Countable.apply[ClassName, Double] _).tupled ).toVector )

    semiauto.deriveReader[ProductionConfig]: @nowarn( "cat=lint-byname-implicit" )
  }

}
