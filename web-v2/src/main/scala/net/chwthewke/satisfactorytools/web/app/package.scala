package net.chwthewke.satisfactorytools
package web

import org.http4s.Charset
import org.http4s.Charset.`UTF-8`
import org.http4s.EntityEncoder
import org.http4s.MediaType
import org.http4s.headers.`Content-Type`
import scalatags.generic.Frag

package object app {
  implicit def scalatagsEncoder[F[_], C <: Frag[_, String]]( implicit
      charset: Charset = `UTF-8`
  ): EntityEncoder[F, C] =
    contentEncoder( MediaType.text.html )

  private def contentEncoder[F[_], C <: Frag[_, String]](
      mediaType: MediaType
  )( implicit charset: Charset ): EntityEncoder[F, C] =
    EntityEncoder
      .stringEncoder[F]
      .contramap[C]( content => "<!DOCTYPE html>\n" + content.render )
      .withContentType( `Content-Type`( mediaType, charset ) )
}
