package net.chwthewke.satisfactorytools
package model

import fr.thomasdufour.autodiff.Diff
import fr.thomasdufour.autodiff.Hint
import fr.thomasdufour.autodiff.derived
import fr.thomasdufour.autodiff.extra.enumeratum._
import scala.collection.immutable.SortedMap
import scala.concurrent.duration.FiniteDuration

import data.ClassName
import data.Item

trait DiffInstances extends data.DiffInstances {
  implicit val machineDiff: Diff[Machine] = {
    import derived.auto._

    derived.semi.diff[Machine]
  }

  implicit val recipeDiff: Diff[Recipe] = {
    import derived.auto._

    implicit val finiteDurationDiff: Diff[FiniteDuration] = Diff.defaultEqShow

    derived.semi.diff[Recipe]
  }

  implicit val resourceOptionsDiff: Diff[ResourceOptions] = {
    import derived.auto._

    derived.semi.diff[ResourceOptions]
  }

  implicit val modelDiff: Diff[Model] = {
    val manufacturingRecipesDiff: Diff[Vector[Recipe]] = {
      implicit val recipeHint: Hint[Recipe] = Hint.byDiff[ClassName].contramap( _.className )
      Diff.inAnyOrder
    }

    val extractedItemsDiff: Diff[Vector[Item]] = {
      implicit val itemHint: Hint[Item] = Hint.byDiff[ClassName].contramap( _.className )
      Diff.inAnyOrder
    }

    Diff.forProduct5( "Model" )(
      "manufacturingRecipes",
      "items",
      "extractedItems",
      "extractionRecipes",
      "defaultResourceOptions"
    )(
      (m: Model) => ( m.manufacturingRecipes, m.items, m.extractedItems, m.extractionRecipes, m.defaultResourceOptions )
    )(
      manufacturingRecipesDiff,
      Diff[SortedMap[ClassName, Item]],
      extractedItemsDiff,
      Diff.inAnyOrder[( Item, ResourcePurity, Recipe ), Vector],
      resourceOptionsDiff
    )

  }

}

object diff extends DiffInstances
