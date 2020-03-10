package net.chwthewke.satisfactorytools

import cats.Reducible
import cats.instances.string._
import cats.syntax.foldable._

case class Error( msg: String ) extends RuntimeException( msg, null, false, false )

object Error {
  def apply[F[_]: Reducible]( errs: F[String] ): Error = Error( errs.intercalate( "," ) )
}
