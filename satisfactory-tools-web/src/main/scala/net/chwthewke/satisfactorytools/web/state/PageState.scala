package net.chwthewke.satisfactorytools
package web.state

import cats.Show
import cats.syntax.either._
import cats.syntax.show._
import org.http4s.FormDataDecoder
import org.http4s.ParseFailure
import scodec.Codec
import scodec.codecs._
import shapeless.Lens
import shapeless.lens

import model.Model
import model.SolverInputs
import prod.Factory
import web.protocol
import web.protocol.Codecs
import web.protocol.Forms

case class PageState(
    inputs: SolverInputs,
    selectedInputTab: InputTab,
    selectedOutputTab: OutputTab,
    factory: Option[Either[String, Factory]],
    customGroupSelection: CustomGroupSelection
)

object PageState {
  def toBase64( model: Model, state: PageState ): Either[String, String] =
    protocol.toBase64( pageStateCodec( model ) )( state )
  def fromBase64( model: Model, base64: String ): Either[String, PageState] =
    protocol.fromBase64( pageStateCodec( model ) )( base64 )

  def pageStateCodec( model: Model ): Codec[PageState] =
    (Codecs.inputsCodec( model ) ~
      protocol.enumCodec[InputTab] ~
      OutputTab.outputTabCodec ~
      optional( bool, either( bool, variableSizeBits( uint16, utf8 ), Codecs.factoryCodec( model ) ) ) ~
      Codecs.customGroupsCodec( model ))
      .xmap(
        PageState( _, _, _, _, _ ),
        state =>
          state.inputs ~ state.selectedInputTab ~ state.selectedOutputTab ~ state.factory ~ state.customGroupSelection
      )

  def formDataDecoder( model: Model ): FormDataDecoder[PageState] =
    FormDataDecoder
      .field[String]( Forms.state )
      .mapValidated(
        str => PageState.fromBase64( model, str ).leftMap( msg => ParseFailure( "", msg ) ).toValidatedNel
      )

  implicit val pageStateShow: Show[PageState] = Show.show(
    state => //
      show"""INPUTS
            |${state.inputs.show.linesIterator.map( "  " + _ ).mkString( "\n" )}
            |
            |INPUT TAB ${state.selectedInputTab.id}
            |
            |OUTPUT TAB ${state.selectedOutputTab.id}
            |
            |FACTORY
            |${state.factory.fold( "Not computed" )( _.fold( identity, _.render( state.inputs.bill ) ) )}
            |""".stripMargin
  )

  val inputsLens: Lens[PageState, SolverInputs]                     = lens[PageState].inputs
  val selectedInputTabLens: Lens[PageState, InputTab]               = lens[PageState].selectedInputTab
  val selectedOutputTabLens: Lens[PageState, OutputTab]             = lens[PageState].selectedOutputTab
  val factoryLens: Lens[PageState, Option[Either[String, Factory]]] = lens[PageState].factory
}
