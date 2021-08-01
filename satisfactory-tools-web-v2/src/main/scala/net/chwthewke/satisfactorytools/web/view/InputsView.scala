package net.chwthewke.satisfactorytools
package web.view

import cats.Order.catsKernelOrderingForOrder
import scalatags.Text
import scalatags.Text.Tag

import data.Countable
import data.Item

object InputsView extends ( ( Vector[Countable[Double, Item]], Int ) => Tag ) {
  import Text.all._

  override def apply( inputs: Vector[Countable[Double, Item]], groupCount: Int ): Tag =
    fieldset(
      legend( "Raw resources" ),
      extractedResourcesView( inputs )
    )

  def extractedResourcesView( res: Vector[Countable[Double, Item]] ): Tag =
    table(
      res
        .filter { case Countable( _, x ) => x.abs > AmountTolerance }
        .sortBy { case Countable( p, x ) => ( -x, p ) }
        .map {
          case Countable( p, x ) =>
            tr(
              numCell4( x ),
              td( p.displayName )
            )
        }
    )
}
