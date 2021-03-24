package net.chwthewke.satisfactorytools
package prod

import cats.syntax.either._
import cats.syntax.option._
import cats.syntax.show._
import cats.syntax.foldable._
import cats.syntax.traverse._
import cats.syntax.traverseFilter._
import mouse.boolean._
//
import net.chwthewke.satisfactorytools.model.Countable
import net.chwthewke.satisfactorytools.model.Item
import net.chwthewke.satisfactorytools.model.Model

final case class Bill( items: Vector[Countable[Item, Double]] )

object Bill {

  def init( config: ProductionConfig, model: Model ): Either[String, Bill] =
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

}
