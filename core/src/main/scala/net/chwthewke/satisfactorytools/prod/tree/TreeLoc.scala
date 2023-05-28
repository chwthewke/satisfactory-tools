package net.chwthewke.satisfactorytools
package prod
package tree

import cats.data.NonEmptyVector
import cats.PartialOrder
import cats.Show
import cats.syntax.foldable._
import cats.syntax.traverse._
import cats.syntax.vector._
import io.circe.Decoder
import io.circe.Encoder

sealed abstract class TreeLoc( val indices: Vector[Int] ) {
  def append( ix: Int ): TreeLoc.NonRoot =
    TreeLoc.NonRoot( indices.toNev.fold( NonEmptyVector.one( ix ) )( _.append( ix ) ) )
  def prepend( ix: Int ): TreeLoc.NonRoot =
    TreeLoc.NonRoot( NonEmptyVector( ix, indices ) )

  def nonRoot: Option[TreeLoc.NonRoot]

  override def toString: String = indices.map( _ + 1 ).mkString_( "." )
}

object TreeLoc {
  def apply( indices: Vector[Int] ): TreeLoc =
    indices.toNev.fold[TreeLoc]( Root )( NonRoot( _ ) )

  def parse( str: String ): Option[TreeLoc] = // TODO cats-parse, come on
    str
      .split( '.' )
      .toVector
      .filter( _.nonEmpty )
      .traverse( _.toIntOption.map( _ - 1 ).filter( _ >= 0 ) )
      .map( TreeLoc( _ ) )

  implicit val treeLocPartialOrder: PartialOrder[TreeLoc] =
    PartialOrder.from(
      ( l, m ) =>
        if (l == m) 0d
        else if (l.indices.startsWith( m.indices )) 1d
        else if (m.indices.startsWith( l.indices )) -1d
        else Double.NaN
    )

  implicit val treeLocShow: Show[TreeLoc] = Show.fromToString

  case object Root extends TreeLoc( Vector.empty ) {
    override def nonRoot: Option[NonRoot] = None
  }
  case class NonRoot( path: NonEmptyVector[Int] ) extends TreeLoc( path.toVector ) {
    def head: Int     = path.head
    def tail: TreeLoc = TreeLoc( path.tail )
    def init: TreeLoc = TreeLoc( path.init )
    def last: Int     = path.last

    override def nonRoot: Option[NonRoot] = Some( this )
  }

  object NonRoot {

    implicit val nonRootDecoder: Decoder[TreeLoc.NonRoot] = Decoder[NonEmptyVector[Int]].map( TreeLoc.NonRoot( _ ) )
    implicit val nonRootEncoder: Encoder[TreeLoc.NonRoot] = Encoder[NonEmptyVector[Int]].contramap( _.path )

    def unapply( loc: NonRoot ): Some[( Int, TreeLoc )] =
      Some( ( loc.head, loc.tail ) )
  }
}
