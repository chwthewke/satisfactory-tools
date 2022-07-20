package net.chwthewke.satisfactorytools

import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.option._
import cats.syntax.show._
import cats.syntax.traverse._
import cats.syntax.traverseFilter._
import mouse.boolean._
import pureconfig.ConfigReader
import pureconfig.generic.semiauto

import data.ClassName
import data.Countable
import model.Bill
import model.Model
import model.RecipeList

final case class ProductionConfig(
    items: Vector[Countable[Double, ClassName]],
    recipes: Vector[ClassName],
    forbidden: Vector[ClassName]
) {
  def allowedRecipes: Vector[ClassName] = recipes.filterNot( forbidden.toSet )

  def mkBill( model: Model ): Either[String, Bill] =
    items
      .traverseFilter {
        case Countable( itemClass, amount ) =>
          (amount != 0d)
            .option(
              model.items
                .get( itemClass )
                .map( Countable( _, amount ) )
                .toValidNel( itemClass.show )
            )
            .sequence
      }
      .toEither
      .leftMap( mi => show"Unknown items in bill: ${mi.intercalate( ", " )}" )
      .map( Bill( _ ) )

  def mkRecipeList( model: Model ): Either[String, RecipeList] =
    allowedRecipes
      .traverse( cn => model.manufacturingRecipes.find( _.className == cn ).toValidNel( cn.show ) )
      .leftMap( missing => show"Unknown recipe(s) in config: ${missing.mkString_( ", " )}" )
      .toEither
      .map( RecipeList( _ ) )
}

object ProductionConfig {

  implicit val productionConfigReader: ConfigReader[ProductionConfig] = {
    import loader.classNameMapReader

    implicit val classNameReader: ConfigReader[ClassName] = ConfigReader[String].map( ClassName( _ ) )
    implicit val itemsReader: ConfigReader[Vector[Countable[Double, ClassName]]] =
      ConfigReader[Map[ClassName, Double]]
        .map( _.map( (Countable.apply[Double, ClassName] _).tupled ).toVector )

    semiauto.deriveReader[ProductionConfig]
  }

}
