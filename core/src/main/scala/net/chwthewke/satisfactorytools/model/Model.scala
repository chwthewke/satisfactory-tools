package net.chwthewke.satisfactorytools
package model

import alleycats.std.iterable._
import cats.Show
import cats.syntax.foldable._
import cats.syntax.show._
import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder
import scala.collection.immutable.SortedMap

import data.ClassName
import data.Item

case class Model(
    version: ModelVersion,
    manufacturingRecipes: Vector[Recipe],
    items: SortedMap[ClassName, Item],
    extractedItems: Vector[Item],
    extractionRecipes: Vector[( Item, ResourcePurity, Recipe )],
    defaultResourceOptions: ResourceOptions
)

object Model {

  implicit val modelShow: Show[Model] = Show.show { model =>
    show"""Manufacturing Recipes
          |${model.manufacturingRecipes.map( _.show ).intercalate( "\n" )}
          |
          |Items
          |${model.items.values.map( _.toString ).intercalate( "\n" )}
          |
          |Extracted Items ${model.extractedItems.map( _.displayName ).intercalate( ", " )}
          |
          |Extraction Recipes
          |${model.extractionRecipes.map( _._3 ).map( _.show ).intercalate( "\n" )}
          |
          |Resource nodes
          |${model.defaultResourceOptions.show.linesIterator.map( "  " + _ ).toSeq.mkString_( "\n" )}
          |""".stripMargin
  }

  implicit val modelDecoder: Decoder[Model] = deriveDecoder[Model]
  implicit val modelEncoder: Encoder[Model] = deriveEncoder[Model]

}
