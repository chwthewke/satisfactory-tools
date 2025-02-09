package net.chwthewke.satisfactorytools
package web.view

import cats.Traverse
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.show._
import scalatags.Text
import scalatags.Text.Tag

import data.Countable
import data.Item
import model.Model
import model.Recipe
import model.RecipeList
import web.forms._

object RecipesView extends ( ( Model, RecipeList ) => Tag ) {

  import Text.all._

  override def apply( model: Model, list: RecipeList ): Tag =
    fieldset(
      legend( "Allowed recipes" ),
      div(
        button(
          `class`    := "button is-info",
          formaction := Actions.addAllRecipes,
          "+ ALL"
        ),
        button(
          `class`    := "button is-info",
          formaction := Actions.addAlts,
          "+ ALL ALTS"
        ),
        button(
          `class`    := "button is-info",
          formaction := Actions.removeAlts,
          "- ALL ALTS"
        ),
        button(
          `class`    := "button is-info",
          formaction := Actions.removeMatterConversion,
          "- MATTER CONV."
        ),
        button(
          `class`    := "button is-info",
          formaction := Actions.lockRecipes,
          "LOCK CURRENT"
        )
      ),
      div(
        span( width := "8em", display.`inline-block`, "WITH ALTS" ),
        9.to( 1, -1 )
          .map( tier =>
            button(
              `class`    := "button is-info",
              formaction := Actions.recipesUpToTier( tier, alternates = true ),
              s"TIER $tier"
            )
          )
      ),
      div(
        span( width := "8em", display.`inline-block`, "NO ALTS" ),
        9.to( 0, -1 )
          .map( tier =>
            button(
              `class`    := "button is-info",
              formaction := Actions.recipesUpToTier( tier, alternates = false ),
              s"TIER $tier"
            )
          )
      ),
      recipeFieldSets( model.manufacturingRecipes, list.recipes.toSet )
    )

  def recipeFieldSets( recipes: Vector[Recipe], selected: Set[Recipe] ): Frag =
    recipes
      .groupBy( _.products.head.item )
      .toVector
      .sortBy( _._1 )
      .map { case ( item, recipes ) => recipeFieldSet( item, recipes, selected ) }

  def recipeFieldSet(
      item: Item,
      recipes: Vector[Recipe],
      selected: Set[Recipe]
  ): Tag =
    fieldset(
      legend( item.displayName ),
      recipes.map( r => recipeField( r, selected( r ) ) )
    )

  def recipeField( recipe: Recipe, selected: Boolean ): Tag = {
    val elId = recipe.className.name
    div(
      input(
        `type` := "checkbox",
        `id`   := elId,
        value  := recipe.className.name,
        name   := Keys.recipes,
        Option.when( selected )( checked )
      ),
      label(
        `for` := elId,
        title := describeRecipe( recipe ),
        recipe.displayName
      )
    )
  }

  def describeRecipe( recipe: Recipe ): String = {
    def showAmount( d: Double ): String =
      if (d.isValidInt) f"${d.toInt}%d"
      else if (d > 1) f"$d%.1f"
      else f"$d%.2f"

    def showItem( item: Countable[Double, Item], perMinute: Countable[Double, Item] ) =
      show"${showAmount( item.amount )} x ${item.item.displayName} @ ${showAmount( perMinute.amount )}/min."

    def showItemList[F[_]: Traverse]( items: F[( Countable[Double, Item], Countable[Double, Item] )] ) =
      items
        .map( ( showItem _ ).tupled )
        .mkString_( ", " )

    val ingredients = showItemList( recipe.ingredients zip recipe.ingredientsPerMinute )
    val products    = showItemList( recipe.products zip recipe.productsPerMinute )

    show"$ingredients \u21d2 $products"
  }

}
