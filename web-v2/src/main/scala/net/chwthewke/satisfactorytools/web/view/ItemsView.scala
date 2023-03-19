package net.chwthewke.satisfactorytools
package web.view

import cats.Order
import cats.Order.catsKernelOrderingForOrder
import scalatags.Text
import scalatags.Text.Tag

import data.Countable
import data.Item
import model.Recipe
import protocol.ItemIO
import protocol.ItemSrcDest

object ItemsView extends ( ( Map[Item, ItemIO[ItemSrcDest]], Int ) => Tag ) {

  import Text.all._

  private def sortAndFilterSmallAmounts[A: Order](
      srcOrDest: Vector[Countable[Double, A]]
  ): Vector[Countable[Double, A]] =
    srcOrDest.filter( _.amount.abs > AmountTolerance ).sortBy( _.item )

  private def sortAndFilterSmallAmounts(
      items: Map[Item, ItemIO[ItemSrcDest]]
  ): Vector[( Item, ItemIO[ItemSrcDest] )] =
    items.toVector
      .sortBy( _._1 )
      .map {
        case ( item, itemIO ) =>
          (
            item,
            ItemIO( sortAndFilterSmallAmounts( itemIO.sources ), sortAndFilterSmallAmounts( itemIO.destinations ) )
          )
      }
      .filterNot {
        case ( _, itemIO ) =>
          itemIO.sources.isEmpty && itemIO.destinations.isEmpty
      }

  private def showRecipeSrcDest( recipe: Recipe ): Modifier =
    Seq[Modifier]( title := RecipesView.describeRecipe( recipe ), recipe.displayName )

  def showItemSrcDest( isd: ItemSrcDest ): Modifier =
    isd match {
      case ItemSrcDest.Extract( recipe ) => showRecipeSrcDest( recipe )
      case ItemSrcDest.Step( recipe, _ ) => showRecipeSrcDest( recipe )
      case ItemSrcDest.Input             => "INPUT"
      case ItemSrcDest.Byproduct         => "BYPRODUCT"
      case ItemSrcDest.Requested         => "REQUESTED"
      case ItemSrcDest.FromGroup( n )    => s"GROUP #$n"
      case ItemSrcDest.ToGroup( n )      => s"GROUP #$n"
    }

  private def itemIORows( dir: String, rows: Vector[Countable[Double, ItemSrcDest]] ): Frag =
    rows.zipWithIndex.map {
      case ( Countable( srcDest, amount ), ix ) =>
        tr(
          numCell4( amount ),
          Option.when( ix == 0 )( td( rowspan := rows.size, verticalAlign.middle, dir ) ),
          td( showItemSrcDest( srcDest ) )
        )
    }

  override def apply( items: Map[Item, ItemIO[ItemSrcDest]], groupCount: Int ): Tag =
    fieldset(
      legend( "Items I/O" ),
      div(
        sortAndFilterSmallAmounts( items ).map {
          case ( item, ItemIO( srcs, dests ) ) =>
            fieldset(
              legend( item.displayName ),
              table(
                tbody(
                  itemIORows( "from", srcs ),
                  itemIORows( "to", dests )
                )
              )
            )
        }
      )
    )
}
