package net.chwthewke.satisfactorytools
package persistence

import cats.Eq
import cats.data.NonEmptyVector
import cats.effect.Sync
import fs2.Chunk
import fs2.Pull
import fs2.Stream
import scala.annotation.tailrec
import scala.reflect.ClassTag

object Streams {
  def groupAdjacentNevWith[F[_]: Sync, A: ClassTag, B, C: ClassTag, K](
      stream: Stream[F, ( A, B, C )],
      f: ( A, B ) => K
  )( implicit EqA: Eq[A] ): Stream[F, ( K, NonEmptyVector[C] )] = {

    def go( current: Option[( A, K, Chunk[C] )], s: Stream[F, ( A, B, C )] ): Pull[F, ( K, NonEmptyVector[C] ), Unit] =
      s.pull.uncons.flatMap {
        case Some( ( hd, tl ) ) =>
          if (hd.isEmpty)
            go( current, tl )
          else {
            def acc: ( A, K, Chunk[C] ) = {
              val abc = hd( 0 )
              ( abc._1, f( abc._1, abc._2 ), Chunk.empty[C] )
            }
            val ( a, k, out ) = current.getOrElse( acc )
            doChunk( hd, tl, a, k, List( out ), None )
          }
        case None =>
          val em = current
            .map {
              case ( _, k, cs ) =>
                Pull.output1[F, ( K, NonEmptyVector[C] )]( ( k, NonEmptyVector.fromVectorUnsafe( cs.toVector ) ) )
            }
            .getOrElse( Pull.pure( () ) )
          em >> Pull.done
      }

    @tailrec
    def doChunk(
        chunk: Chunk[( A, B, C )],
        s: Stream[F, ( A, B, C )],
        a: A,
        k: K,
        out: List[Chunk[C]],
        acc: Option[Chunk[( K, Chunk[C] )]]
    ): Pull[F, ( K, NonEmptyVector[C] ), Unit] = {
      chunk.indexWhere( v => EqA.neqv( v._1, a ) ) match {
        case Some( ix ) =>
          val matching    = chunk.take( ix )
          val newOut      = matching.map( _._3 ) :: out
          val nonMatching = chunk.drop( ix ) // cannot be empty (ix < chunk.size)
          val ( a1, k1 ) = {
            val abc = nonMatching( 0 )
            ( abc._1, f( abc._1, abc._2 ) )
          }
          val newAcc = Chunk.concat( acc.toList ::: List( Chunk( ( k, Chunk.concat( newOut.reverse ) ) ) ) )

          doChunk(
            nonMatching,
            s,
            a1,
            k1,
            Nil,
            Some( newAcc )
          )
        case None =>
          val newOut = chunk.map( _._3 ) :: out
          acc match {
            case Some( kcs ) =>
              Pull.output( kcs.map { case ( k, cs ) => ( k, NonEmptyVector.fromVectorUnsafe( cs.toVector ) ) } ) >>
                go( Some( ( a, k, Chunk.concat( newOut.reverse ) ) ), s )
            case None =>
              go( Some( ( a, k, Chunk.concat( newOut.reverse ) ) ), s )
          }
      }

    }

    go( None, stream ).stream
  }

  def groupAdjacentByFirstNev[F[_]: Sync, A: Eq: ClassTag, B: ClassTag](
      stream: Stream[F, ( A, B )]
  ): Stream[F, ( A, NonEmptyVector[B] )] =
    groupAdjacentNevWith[F, A, Unit, B, A]( stream.map { case ( a, b ) => ( a, (), b ) }, ( a, _ ) => a )

  def groupAdjacentRows[F[_]: Sync, A: Eq: ClassTag, B, C: ClassTag](
      stream: Stream[F, ( A, B, C )]
  ): Stream[F, ( A, B, NonEmptyVector[C] )] =
    groupAdjacentNevWith[F, A, B, C, ( A, B )]( stream, ( a, b ) => ( a, b ) ).map {
      case ( ( a, b ), cs ) => ( a, b, cs )
    }
}
