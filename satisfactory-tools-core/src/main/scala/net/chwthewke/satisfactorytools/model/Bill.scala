package net.chwthewke.satisfactorytools
package model

import cats.Show
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.option._
import cats.syntax.show._
import cats.syntax.traverse._
import cats.syntax.traverseFilter._
import mouse.boolean._

import data.ProductionConfig

final case class Bill( items: Vector[Countable[Double, Item]] ) {
  def amountOf( item: Item ): Double =
    items.find( _.item == item ).map( _.amount ).orEmpty
}

object Bill {

  def init( model: Model, config: ProductionConfig ): Either[String, Bill] =
    config.items
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

  implicit val billShow: Show[Bill] = Show.show(
    _.items
      .map { case Countable( item, amount ) => f"$amount%.2f ${item.displayName}" }
      .mkString( "\n" )
  )

}
