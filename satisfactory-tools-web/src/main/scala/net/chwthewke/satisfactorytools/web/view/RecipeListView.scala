package net.chwthewke.satisfactorytools
package web.view

import cats.Order.catsKernelOrderingForOrder
import cats.Traverse
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.show._
import scalatags.Text
import scalatags.Text.all._

import model.Countable
import model.Item
import model.Model
import model.Recipe
import model.RecipeList
import web.protocol.Forms

object RecipeListView {

  def view( model: Model, recipes: RecipeList ): Text.TypedTag[String] =
    fieldset(
      legend( "Allowed recipes" ),
      recipeFieldSets( model.manufacturingRecipes, recipes.recipes.toSet )
    )

  def recipeFieldSets[M]( recipes: Vector[Recipe[M, Item]], selected: Set[Recipe[M, Item]] ): Frag =
    recipes
      .groupBy( _.product.head.item )
      .toVector
      .sortBy( _._1 )
      .map { case ( item, recipes ) => recipeFieldSet( item, recipes, selected ) }

  def recipeFieldSet[M](
      item: Item,
      recipes: Vector[Recipe[M, Item]],
      selected: Set[Recipe[M, Item]]
  ): Text.TypedTag[String] =
    fieldset(
      legend( item.displayName ),
      recipes.map( r => recipeField( r, selected( r ) ) )
    )

  def recipeField[M]( recipe: Recipe[M, Item], selected: Boolean ): Text.TypedTag[String] = {
    val elId = recipe.className.name
    div(
      input(
        `type` := "checkbox",
        `id` := elId,
        value := recipe.className.name,
        name := Forms.recipes,
        Option.when( selected )( checked )
      ),
      label(
        `for` := elId,
        title := describeRecipe( recipe ),
        recipe.displayName
      )
    )
  }

  def describeRecipe[M]( recipe: Recipe[M, Item] ): String = {
    def showAmount( d: Double ): String =
      if (d.isValidInt) f"${d.toInt}%d"
      else if (d > 1) f"$d%.1f"
      else f"$d%.2f"

    def showItem( item: Countable[Double, Item], perMinute: Countable[Double, Item] ) =
      show"${showAmount( item.amount )} x ${item.item.displayName} @ ${showAmount( perMinute.amount )}/min."

    def showItemList[F[_]: Traverse]( items: F[( Countable[Double, Item], Countable[Double, Item] )] ) =
      items
        .map( (showItem _).tupled )
        .mkString_( ", " )

    val ingredients = showItemList( recipe.ingredients zip recipe.ingredientsPerMinute )
    val products    = showItemList( recipe.product zip recipe.productsPerMinute )

    show"$ingredients \u21d2 $products"
  }

}