package net.chwthewke.satisfactorytools
package model

import alleycats.std.iterable._
import cats.Show
import cats.syntax.foldable._
import cats.syntax.show._
import scala.collection.immutable.SortedMap

case class Model(
    manufacturingRecipes: Vector[Recipe[Machine, Item]],
    items: SortedMap[ClassName, Item],
    extractedItems: Vector[Item],
    extractionRecipes: Vector[( Item, ResourcePurity, Recipe[Machine, Item] )],
    defaultMapOptions: MapOptions
)

object Model {

  implicit val modelShow: Show[Model] = Show.show { model =>
    implicit val showItem: Show[Item]       = Show.show( _.displayName )
    implicit val showMachine: Show[Machine] = Show.show( _.displayName )

    show"""Manufacturing Recipes
          |${model.manufacturingRecipes.map( _.show ).intercalate( "\n" )}
          |
          |Items
          |${model.items.values.map( _.toString ).intercalate( "\n" )}
          |
          |Extracted Items ${model.extractedItems.map( _.displayName ).intercalate( ", " )}
          |
          |Extraction Recipes
          |${model.extractionRecipes.map( _._2 ).map( _.show ).intercalate( "\n" )}
          |
          |Resource nodes
          |${model.defaultMapOptions.show.linesIterator.map( "  " + _ ).toSeq.mkString_( "\n" )}
          |""".stripMargin
  }

}
