package net.chwthewke.satisfactorytools
package model

import alleycats.std.iterable._
import cats.Show
import cats.syntax.foldable._
import cats.syntax.show._

case class Model(
    manufacturingRecipes: Vector[Recipe[Machine, Item]],
    items: Map[ClassName, Item],
    extractedItems: Vector[Item],
    extractionRecipes: Vector[( Item, Recipe[Machine, Item] )] // TODO can we make these Recipe[Extractor, Item]? useful?
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
          |""".stripMargin
  }

}
