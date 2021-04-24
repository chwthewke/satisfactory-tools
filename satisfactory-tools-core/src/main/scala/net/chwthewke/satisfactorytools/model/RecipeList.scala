package net.chwthewke.satisfactorytools
package model

import cats.syntax.foldable._
import cats.syntax.option._
import cats.syntax.show._
import cats.syntax.traverse._

import data.ProductionConfig

case class RecipeList( recipes: Vector[Recipe[Machine, Item]] )

object RecipeList {
  def init( model: Model, config: ProductionConfig ): Either[String, RecipeList] =
    config.allowedRecipes
      .traverse( cn => model.manufacturingRecipes.find( _.className == cn ).toValidNel( cn.show ) )
      .leftMap( missing => show"Unknown recipe(s) in config: ${missing.mkString_( ", " )}" )
      .toEither
      .map( RecipeList( _ ) )

}
