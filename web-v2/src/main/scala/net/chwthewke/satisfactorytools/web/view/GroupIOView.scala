package net.chwthewke.satisfactorytools
package web.view

import cats.Order.catsKernelOrderingForOrder
import cats.syntax.foldable._
import cats.syntax.functorFilter._
import scala.collection.immutable.SortedSet
import scalatags.Text
import scalatags.Text.Tag

import data.Countable
import data.Item
import protocol.ItemIO
import protocol.ItemSrcDest

object GroupIOView extends ( ( Map[Item, ItemIO[ItemSrcDest.InterGroup]], Int ) => Tag ) {
  import Text.all._

  private case class Page(
      items: Map[Item, ItemIO[ItemSrcDest.InterGroup]],
      groups: Map[ItemSrcDest.InterGroup, Map[Item, Double]],
      groupNumbers: SortedSet[Int]
  ) {
    private def extractPeers(
        loc: Option[ItemSrcDest.InterGroup],
        peers: ItemIO[ItemSrcDest.InterGroup] => Vector[Countable[Double, ItemSrcDest.InterGroup]]
    ): Vector[( Item, Double, Vector[Countable[Double, ItemSrcDest.InterGroup]] )] =
      loc
        .flatMap( groups.get )
        .foldMap(
          _.toVector
            .mapFilter {
              case ( item, amount ) =>
                Option.when( amount.abs > AmountTolerance )(
                  (
                    item,
                    amount,
                    ItemsView.sortAndFilterSmallAmounts( items.get( item ).foldMap( peers ) )
                  )
                )
            }
            .sortBy( _._1 )
        )

    private def peersTable(
        dir: String,
        peerings: Vector[( Item, Double, Vector[Countable[Double, ItemSrcDest.InterGroup]] )]
    ): Tag =
      table(
        peerings.flatMap {
          case ( item, amount, peers ) =>
            def itemCells( height: Int ): Frag = Seq[Frag](
              numCell4( amount )( rowspan := height, verticalAlign.middle ),
              td( rowspan := height, verticalAlign.middle, item.displayName )
            )

            if (peers.isEmpty)
              Vector( tr( itemCells( 1 ) ) )
            else
              peers.zipWithIndex.map {
                case ( peer, ix ) =>
                  tr(
                    Seq[Frag](
                      Option.when( ix == 0 )(
                        Seq[Frag](
                          itemCells( peers.size ),
                          td( rowspan := peers.size, verticalAlign.middle, dir )
                        )
                      ),
                      td( ItemsView.showItemSrcDest( peer.item ) ),
                      numCell4( peer.amount )
                    )
                  )
              }
        }
      )

    private def block(
        title: String,
        src: Option[ItemSrcDest.InterGroup],
        dest: Option[ItemSrcDest.InterGroup]
    ): Frag = {
      val inputs: Vector[( Item, Double, Vector[Countable[Double, ItemSrcDest.InterGroup]] )] =
        extractPeers( dest, _.sources )

      val outputs: Vector[( Item, Double, Vector[Countable[Double, ItemSrcDest.InterGroup]] )] =
        extractPeers( src, _.destinations )

      Option.when( inputs.nonEmpty || outputs.nonEmpty )(
        fieldset(
          legend( title ),
          Option.when( inputs.nonEmpty )( Seq( h4( "INPUTS" ), peersTable( "from", inputs ) ) ),
          Option.when( outputs.nonEmpty )( Seq( h4( "OUTPUTS" ), peersTable( "to", outputs ) ) )
        )
      )
    }

    def groupBlock( n: Int ): Frag =
      block( s"Group #$n", Some( ItemSrcDest.FromGroup( n ) ), Some( ItemSrcDest.ToGroup( n ) ) )

    def allBlocks: Frag =
      Seq( block( "Extraction", Some( ItemSrcDest.Input ), None ) ) ++
        groupNumbers.toSeq.map( groupBlock ) ++
        Seq(
          block( "Requested", None, Some( ItemSrcDest.Requested ) ),
          block( "Byproducts", None, Some( ItemSrcDest.Byproduct ) )
        )

  }

  private object Page {
    private def groups(
        byGroup: Map[ItemSrcDest.InterGroup, Map[Item, Double]]
    ): SortedSet[Int] =
      byGroup.keySet
        .collect {
          case ItemSrcDest.FromGroup( n ) => n
          case ItemSrcDest.ToGroup( n )   => n
        }
        .to( SortedSet )

    private def byGroup(
        items: Map[Item, ItemIO[ItemSrcDest.InterGroup]]
    ): Map[ItemSrcDest.InterGroup, Map[Item, Double]] =
      items.toVector.foldMap {
        case ( item, itemIO ) =>
          (itemIO.sources ++ itemIO.destinations).foldMap {
            case Countable( srcDest, amount ) =>
              Map( srcDest -> Map( item -> amount ) )
          }
      }

    def apply( items: Map[Item, ItemIO[ItemSrcDest.InterGroup]] ): Page = {
      val bg = byGroup( items )
      val gs = groups( bg )
      Page( items, bg, gs )
    }
  }

  override def apply( items: Map[Item, ItemIO[ItemSrcDest.InterGroup]], groupCount: Int ): Text.Tag =
    fieldset(
      legend( "Items I/O" ),
      div( Page( items ).allBlocks )
    )
}
