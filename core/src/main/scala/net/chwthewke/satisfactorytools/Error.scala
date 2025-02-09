package net.chwthewke.satisfactorytools

import cats.Reducible
import cats.syntax.all._

case class Error( msg: String ) extends RuntimeException( msg, null, false, false )

object Error {
  def apply[F[_]: Reducible]( errs: F[String] ): Error = Error( errs.intercalate( "," ) )
}
