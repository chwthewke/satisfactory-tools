package net.chwthewke.satisfactorytools
package model

import alleycats.std.iterable._
import cats.Show
import cats.instances.string._
import cats.instances.vector._
import cats.syntax.foldable._
import cats.syntax.show._

case class Model( recipes: Vector[Recipe[Machine, Item]], items: Map[ClassName, Item], extractedItems: Vector[Item] )

object Model {
  implicit val modelShow: Show[Model] = Show.show { model =>
    implicit val showItem: Show[Item]       = Show.show( _.displayName )
    implicit val showMachine: Show[Machine] = Show.show( _.displayName )

    show"""Recipes
          |${model.recipes.map( _.show ).intercalate( "\n" )}
          |
          |Items
          |${model.items.values.map( _.show ).intercalate( "\n" )}
          |""".stripMargin
  }

}
