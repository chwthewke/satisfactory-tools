package net.chwthewke.satisfactorytools
package web.state

import cats.Show
import cats.derived.semiauto
import cats.syntax.either._
import org.http4s.FormDataDecoder
import org.http4s.ParseFailure
import scodec.Codec
import scodec.codecs

import model.Model
import model.SolverInputs
import web.protocol
import web.protocol.Codecs
import web.protocol.FormNames

case class PageState(
    inputs: SolverInputs,
    selectedInputTab: InputTab,
    compute: Boolean
)

object PageState {
  def toBase64( model: Model, state: PageState ): String =
    protocol.toBase64( pageStateCodec( model ) )( state )
  def fromBase64( model: Model, base64: String ): Either[String, PageState] =
    protocol.fromBase64( pageStateCodec( model ) )( base64 )

  def pageStateCodec( model: Model ): Codec[PageState] =
    (Codecs.inputsCodec( model ) ~~ protocol.enumCodec[InputTab] ~~ codecs.bool)
      .xmap( (PageState.apply _).tupled, state => ( state.inputs, state.selectedInputTab, state.compute ) )

  def formDataDecoder( model: Model ): FormDataDecoder[PageState] =
    FormDataDecoder
      .field[String]( FormNames.state )
      .mapValidated(
        str => PageState.fromBase64( model, str ).leftMap( msg => ParseFailure( "", msg ) ).toValidatedNel
      )

  implicit val pageStateShow: Show[PageState] = semiauto.show[PageState]
}
