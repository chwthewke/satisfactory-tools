package net.chwthewke.vendor.io.circe.fs2

import _root_.fs2.Chunk
import _root_.fs2.Pipe
import _root_.fs2.Pull
import _root_.fs2.RaiseThrowable
import _root_.fs2.Stream
import cats.effect.Sync
import io.circe.Json
import io.circe.ParsingFailure
import io.circe.jawn.CirceSupportParser
import org.typelevel.jawn.AsyncParser
import org.typelevel.jawn.ParseException
import scala.collection.Seq

private[fs2] abstract class ParsingPipe[F[_]: Sync, S]( supportParser: CirceSupportParser ) extends Pipe[F, S, Json] {
  private[this] val raiseThrowable: RaiseThrowable[F] = RaiseThrowable.fromApplicativeError

  protected[this] def parsingMode: AsyncParser.Mode

  protected[this] def parseWith( parser: AsyncParser[Json] )( in: S ): Either[ParseException, Seq[Json]]

  private[this] final def makeParser: F[AsyncParser[Json]] = Sync[F].delay( supportParser.async( mode = parsingMode ) )

  private[this] final def doneOrLoop[A]( p: AsyncParser[Json] )( s: Stream[F, S] ): Pull[F, Json, Unit] =
    s.pull.uncons1.flatMap {
      case Some( ( s, str ) ) =>
        parseWith( p )( s ) match {
          case Left( error ) =>
            Pull.raiseError( ParsingFailure( error.getMessage, error ) )( raiseThrowable )
          case Right( js ) =>
            Pull.output( Chunk.seq( js ) ) >> doneOrLoop( p )( str )
        }
      case None => Pull.done
    }

  final def apply( s: Stream[F, S] ): Stream[F, Json] =
    Stream.eval( makeParser ).flatMap( parser => doneOrLoop( parser )( s ).stream )
}
