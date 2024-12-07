package net.chwthewke.satisfactorytools
package web.view

import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.functor._
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

  case class Block(
      title: String,
      asSrc: Option[ItemSrcDest.InterGroup with ItemSrcDest.ItemSrc],
      asDest: Option[ItemSrcDest.InterGroup with ItemSrcDest.ItemDest]
  )

  private case class Page(
      items: Map[Item, ItemIO[ItemSrcDest.InterGroup]],
      groups: Map[ItemSrcDest.InterGroup, Map[Item, Double]],
      groupNumbers: SortedSet[Int]
  ) {

    def groupBlock( n: Int ): Block =
      Block( s"Group #$n", Some( ItemSrcDest.FromGroup( n ) ), Some( ItemSrcDest.ToGroup( n ) ) )

    val allBlocks: Vector[Block] =
      Vector( Block( "Extraction", Some( ItemSrcDest.Input ), None ) ) ++
        groupNumbers.toSeq.map( groupBlock ) ++
        Seq(
          Block( "Requested", None, Some( ItemSrcDest.Requested ) ),
          Block( "Byproducts", None, Some( ItemSrcDest.Byproduct ) )
        )

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
        borderCollapse.collapse,
        peerings.zipWithIndex.flatMap {
          case ( ( item, amount, peers ), outerIx ) =>
            def itemCells( height: Int ): Frag = Seq[Frag](
              numCell4( amount )( rowspan := height, verticalAlign.middle, paddingRight := "0.5em" ),
              td( rowspan                 := height, verticalAlign.middle, strong( item.displayName ) )
            )

            if (peers.isEmpty)
              Vector( tr( itemCells( 1 ) ) )
            else
              peers.zipWithIndex.map {
                case ( peer, ix ) =>
                  tr(
                    Option.when( outerIx > 0 && ix == 0 )(
                      borderTop := "1px solid"
                    ),
                    Option.when( ix == 0 )(
                      Seq[Frag](
                        itemCells( peers.size ),
                        td( rowspan := peers.size, verticalAlign.middle, dir, padding := "0 1em" )
                      )
                    ),
                    td( ItemsView.showItemSrcDest( peer.item ) ),
                    numCell4( peer.amount )
                  )
              }
        }
      )

    def srcDestTables( block: Block ): Frag = {

      def extractFromTo(
          from: Option[ItemSrcDest.InterGroup with ItemSrcDest.ItemSrc],
          to: Option[ItemSrcDest.InterGroup with ItemSrcDest.ItemDest]
      ): Vector[( Item, Double, Double )] =
        ( from, to ).tupled
          .foldMap {
            case ( src, dest ) =>
              items.to( Vector ).mapFilter {
                case ( item, srcDests ) =>
                  (
                    srcDests.sources.find( c => c.item == src && c.amount.abs > AmountTolerance ).map( _.amount ),
                    srcDests.destinations.find( c => c.item == dest && c.amount.abs > AmountTolerance ).map( _.amount )
                  ).mapN( ( item, _, _ ) )
              }
          }

      def tableWithPeer(
          peer: Vector[( Item, Double, Double )],
          withTotal: Boolean
      ): Frag =
        table(
          borderCollapse.collapse,
          peer.zipWithIndex.map {
            case ( ( item, prod, cons ), ix ) =>
              tr(
                Option.when( ix > 0 )( borderTop                     := "1px solid" ),
                numCell4( cons )( verticalAlign.middle, paddingRight := "0.5em" ),
                td( strong( item.displayName ), verticalAlign.middle ),
                td( "of", verticalAlign.middle, padding := "0 1em" ),
                td( numCell4( prod ), verticalAlign.middle )
              )
          } ++
            Option.when( withTotal )(
              tr(
                borderTop := "1px solid",
                numCell4( peer.foldMap( _._3 ) ),
                td( colspan := "3", "Total" )
              )
            )
        )

      def tablesWithPeer(
          other: Block,
          fromOther: Vector[( Item, Double, Double )],
          toOther: Vector[( Item, Double, Double )]
      ): Frag =
        Seq[Frag](
          h3( other.title ),
          Option.when( fromOther.nonEmpty )(
            Seq[Frag](
              other.asDest.as( h4( "INPUTS" ) ),
              tableWithPeer( fromOther, withTotal = false )
            )
          ),
          Option.when( toOther.nonEmpty )(
            Seq[Frag](
              other.asSrc.as( h4( "OUTPUTS" ) ),
              tableWithPeer(
                toOther,
                withTotal =
                  other.asDest.contains( ItemSrcDest.Requested ) || other.asDest.contains( ItemSrcDest.Byproduct )
              )
            )
          )
        )

      allBlocks
        .filter( _ != block )
        .mapFilter { other =>
          val fromOther: Vector[( Item, Double, Double )] = extractFromTo( other.asSrc, block.asDest )
          val toOther: Vector[( Item, Double, Double )]   = extractFromTo( block.asSrc, other.asDest )

          Option.when( fromOther.nonEmpty || toOther.nonEmpty )(
            tablesWithPeer( other, fromOther, toOther )
          )
        }
    }

    private def blockTag( block: Block ): Frag = {
      val inputs: Vector[( Item, Double, Vector[Countable[Double, ItemSrcDest.InterGroup]] )] =
        extractPeers( block.asDest, _.sources )

      val outputs: Vector[( Item, Double, Vector[Countable[Double, ItemSrcDest.InterGroup]] )] =
        extractPeers( block.asSrc, _.destinations )

      Option.when( inputs.nonEmpty || outputs.nonEmpty )(
        fieldset(
          legend( block.title ),
          display.flex,
          flexDirection.row,
          flexWrap.nowrap,
          div(
            flexGrow := "1",
            Option.when( inputs.nonEmpty )( Seq( h4( "INPUTS" ), peersTable( "from", inputs ) ) ),
            Option.when( outputs.nonEmpty )( Seq( h4( "OUTPUTS" ), peersTable( "to", outputs ) ) )
          ),
          div(
            flexGrow := "1",
            srcDestTables( block )
          )
        )
      )
    }

    def allBlockTags: Frag = allBlocks.map( blockTag )

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
          ( itemIO.sources ++ itemIO.destinations ).foldMap {
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
      div( Page( items ).allBlockTags )
    )
}
