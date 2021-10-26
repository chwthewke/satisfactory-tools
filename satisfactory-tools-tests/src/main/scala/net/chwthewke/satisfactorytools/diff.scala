package net.chwthewke.satisfactorytools

import cats.data.NonEmptyList
import cats.syntax.show._
import fr.thomasdufour.autodiff.Diff
import fr.thomasdufour.autodiff.Hint
import fr.thomasdufour.autodiff.derived
import fr.thomasdufour.autodiff.extra.enumeratum._
import scala.collection.immutable.SortedMap
import scala.concurrent.duration.FiniteDuration

import data.ClassName
import data.Countable
import data.GameData
import data.GameRecipe
import data.Item
import model.Model
import model.Recipe
import model.ResourceOptions
import model.ResourcePurity

trait DiffInstances {

  val Tolerance = 1e-9

  private def doubleEq( x: Double, y: Double ): Boolean = {
    val mag = x.abs.max( y.abs )

    (x - y).abs < Tolerance * mag || mag * Tolerance < Double.MinPositiveValue
  }

  private implicit val doubleDiff: Diff[Double] =
    Diff.explicitEqShow( doubleEq, _.show )

  private implicit val finiteDurationDiff: Diff[FiniteDuration] = Diff.defaultEqShow

  implicit val classNameDiff: Diff[ClassName] = Diff[String].contramap( _.name )

  implicit val itemDiff: Diff[Item] = {

    derived.semi.diff[Item]
  }

  implicit def countableDiff[N: Diff, A: Diff]: Diff[Countable[N, A]] = {

    derived.semi.diff[Countable[N, A]]
  }

  implicit val gameRecipeDiff: Diff[GameRecipe] = {
    import derived.auto._

    implicit def countableHint[A]: Hint[Countable[A, ClassName]] =
      Hint.byDiff[ClassName].contramap( _.item )

    implicit def ingredientsDiff: Diff[List[Countable[Double, ClassName]]] =
      Diff.inAnyOrder[Countable[Double, ClassName], List]

    implicit def productsDiff: Diff[NonEmptyList[Countable[Double, ClassName]]] =
      Diff.inAnyOrder[Countable[Double, ClassName], NonEmptyList]

    derived.semi.diff[GameRecipe]
  }

  implicit val gameRecipesDiff: Diff[Vector[GameRecipe]] = {
    implicit val gameRecipeHint: Hint[GameRecipe] =
      Hint.byDiff[ClassName].contramap( _.className )

    Diff.inAnyOrder[GameRecipe, Vector]
  }

  implicit val gameDataDiff: Diff[GameData] = {
    import derived.auto._

    derived.semi.diff[GameData]
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
