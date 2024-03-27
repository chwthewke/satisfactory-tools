package net.chwthewke.satisfactorytools
package prod
package planning

import cats.syntax.option._
import cats.syntax.traverse._

import data.Countable
import data.Item
import model.Recipe

sealed abstract class Process extends Product {
  def parent: Option[ProcessId]

  def producedPerMinute: Vector[Countable[Double, Item]]
  def consumedPerMinute: Vector[Countable[Double, Item]]

  final def produced( item: Item ): Double =
    producedPerMinute.find( _.item == item ).map( _.amount ).orEmpty
  final def consumed( item: Item ): Double =
    consumedPerMinute.find( _.item == item ).map( _.amount ).orEmpty

  def amountOf( item: Item ): Countable[Double, Item] =
    Countable( item, produced( item ) - consumed( item ) )
}

object Process {
  case class Manufacturing( recipe: Countable[Double, Recipe] ) extends Process {
    override val parent: Option[ProcessId] = None
    override def producedPerMinute: Vector[Countable[Double, Item]] =
      recipe.flatTraverse( _.productsPerMinute ).toList.toVector

    override def consumedPerMinute: Vector[Countable[Double, Item]] =
      recipe.flatTraverse( _.ingredientsPerMinute ).toVector
  }

  case class Extraction( recipe: ClockedRecipe ) extends Process {
    override val parent: Option[ProcessId] = None
    override def producedPerMinute: Vector[Countable[Double, Item]] =
      recipe.productsPerMinute.toList.toVector

    override def consumedPerMinute: Vector[Countable[Double, Item]] =
      Vector.empty
  }

  case class Request( requested: Countable[Double, Item] ) extends Process {
    override val parent: Option[ProcessId] = None
    override def producedPerMinute: Vector[Countable[Double, Item]] =
      Vector.empty

    override def consumedPerMinute: Vector[Countable[Double, Item]] =
      Vector( requested )
  }

  case class ByProduct( byProduct: Countable[Double, Item] ) extends Process {
    override val parent: Option[ProcessId] = None
    override def producedPerMinute: Vector[Countable[Double, Item]] =
      Vector.empty

    override def consumedPerMinute: Vector[Countable[Double, Item]] =
      Vector( byProduct )
  }
}
