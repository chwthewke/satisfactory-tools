package net.chwthewke.satisfactorytools
package data

import pureconfig.ConfigReader
import pureconfig.generic.semiauto
import scala.annotation.nowarn

import model.ClassName
import model.Countable

final case class ProductionConfig(
    items: Vector[Countable[Double, ClassName]],
    recipes: Vector[ClassName],
    forbidden: Vector[ClassName]
) {
  def allowedRecipes: Vector[ClassName] = recipes.filterNot( forbidden.toSet )
}

object ProductionConfig {

  implicit val productionConfigReader: ConfigReader[ProductionConfig] = {

    implicit val classNameReader: ConfigReader[ClassName] = ConfigReader[String].map( ClassName( _ ) )
    implicit val itemsReader: ConfigReader[Vector[Countable[Double, ClassName]]] =
      ConfigReader[Map[ClassName, Double]]
        .map( _.map( (Countable.apply[Double, ClassName] _).tupled ).toVector )

    semiauto.deriveReader[ProductionConfig]: @nowarn( "cat=lint-byname-implicit" )
  }

}
