package net.chwthewke.satisfactorytools
package protocol

import cats.Monoid
import cats.Show
import cats.syntax.foldable._
import cats.syntax.show._
import cats.syntax.traverse._
import scala.annotation.nowarn

import data.Countable

class ItemIO[+A <: ItemSrcDest](
    val sources: Vector[Countable[Double, A with ItemSrcDest.ItemSrc]],
    val destinations: Vector[Countable[Double, A with ItemSrcDest.ItemDest]]
) {
  @nowarn( "msg=unreachable code" )
  def cancelOpposites: ItemIO[A] = {
    val ( cancelledSources, cancelledDestinations ) =
      sources.foldLeft( ( Vector.empty[Countable[Double, A with ItemSrcDest.ItemSrc]], destinations ) ) {
        case ( ( srcAcc, destAcc ), src ) =>
          src.item match {
            case ItemSrcDest.FromGroup( n ) =>
              val ( cancelled, nextDest ) = destAcc.traverse {
                case c @ Countable( ItemSrcDest.ToGroup( `n` ), amt ) =>
                  val cancelled: Double = amt.min( src.amount )
                  ( cancelled, c.copy( amount = amt - cancelled ) )
                case other =>
                  ( 0d, other )
              }

              ( srcAcc :+ src.copy( amount = src.amount - cancelled ), nextDest )
            case _ => ( srcAcc :+ src, destAcc )
          }
      }

    new ItemIO( cancelledSources, cancelledDestinations )
  }

}

object ItemIO {

  def apply[A <: ItemSrcDest](
      sources: Vector[Countable[Double, A with ItemSrcDest.ItemSrc]],
      destinations: Vector[Countable[Double, A with ItemSrcDest.ItemDest]]
  ): ItemIO[A] = new ItemIO( sources, destinations ).cancelOpposites

  def unapply[A <: ItemSrcDest]( itemIO: ItemIO[A] ): Some[
    (
        Vector[Countable[Double, A with ItemSrcDest.ItemSrc]],
        Vector[Countable[Double, A with ItemSrcDest.ItemDest]]
    )
  ] = Some( ( itemIO.sources, itemIO.destinations ) )

  implicit def itemIOShow[A <: ItemSrcDest: Show]: Show[ItemIO[A]] = Show.show {
    case ItemIO( sources, destinations ) =>
      def showList( v: Vector[Countable[Double, A]] ): String = v.mkString_( ", " )
      show"ItemIO( sources = ${showList( sources )}, destinations = ${showList( destinations )})"
  }

  implicit def itemIOMonoid[A <: ItemSrcDest]: Monoid[ItemIO[A]] = new Monoid[ItemIO[A]] {
    override def empty: ItemIO[A] =
      ItemIO( Vector.empty, Vector.empty )

    override def combine( x: ItemIO[A], y: ItemIO[A] ): ItemIO[A] = {
      val sources: Vector[Countable[Double, A with ItemSrcDest.ItemSrc]] =
        ( x.sources ++ y.sources ).gather
      val destinations: Vector[Countable[Double, A with ItemSrcDest.ItemDest]] =
        ( x.destinations ++ y.destinations ).gather
      ItemIO( sources, destinations ).cancelOpposites
    }
  }

  def in[A <: ItemSrcDest with ItemSrcDest.ItemSrc]( src: A, amount: Double ): ItemIO[A] =
    ItemIO( Vector( Countable( src, amount ) ), Vector.empty )

  def out[A <: ItemSrcDest with ItemSrcDest.ItemDest]( dest: A, amount: Double ): ItemIO[A] =
    ItemIO( Vector.empty, Vector( Countable( dest, amount ) ) )
}
