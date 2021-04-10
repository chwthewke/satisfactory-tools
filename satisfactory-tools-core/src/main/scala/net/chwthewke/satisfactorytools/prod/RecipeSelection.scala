package net.chwthewke.satisfactorytools
package prod

import cats.syntax.foldable._
import cats.syntax.option._
import cats.syntax.show._
import cats.syntax.traverse._

import data.ProductionConfig
import model.Item
import model.Machine
import model.Model
import model.Options
import model.Recipe

case class RecipeSelection(
    allowedRecipes: Vector[Recipe[Machine, Item]],
    extractionRecipes: Map[Item, Recipe[Machine, Item]]
)

object RecipeSelection {

  def extractionRecipes( model: Model, options: Options ): Map[Item, Recipe[Machine, Item]] =
    model.extractionRecipes.filter { case ( _, recipe ) => options.allowsExtractionRecipe( recipe ) }.toMap

  def init( model: Model, config: ProductionConfig, options: Options ): Either[String, RecipeSelection] =
    config.recipes
      .traverse( cn => model.manufacturingRecipes.find( _.className == cn ).toValidNel( cn.show ) )
      .leftMap( missing => show"Unknown recipe(s) in config: ${missing.mkString_( ", " )}" )
      .toEither
      .map( RecipeSelection( _, extractionRecipes( model, options ) ) )
}
