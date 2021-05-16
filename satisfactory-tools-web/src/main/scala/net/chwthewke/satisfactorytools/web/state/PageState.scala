package net.chwthewke.satisfactorytools
package web.state

import cats.Show
import cats.data.ValidatedNel
import cats.syntax.either._
import cats.syntax.show._
import cats.syntax.traverse._
import org.http4s.FormDataDecoder
import org.http4s.ParseFailure
import scodec.Codec
import scodec.codecs._
import shapeless.Lens
import shapeless.lens

import model.Model
import prod.Factory
import prod.SolverInputs
import text.FactoryTable
import web.protocol
import web.protocol.Codecs

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

  def validate( model: Model, str: String ): ValidatedNel[ParseFailure, PageState] =
    PageState.fromBase64( model, str ).leftMap( msg => ParseFailure( "", msg ) ).toValidatedNel

  def formDataDecoder( model: Model, fieldName: String ): FormDataDecoder[PageState] =
    FormDataDecoder
      .field[String]( fieldName )
      .mapValidated( validate( model, _ ) )

  def formDataDecoderOpt( model: Model, fieldName: String ): FormDataDecoder[Option[PageState]] =
    FormDataDecoder
      .fieldOptional[String]( fieldName )
      .mapValidated( _.traverse( validate( model, _ ) ) )

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
            |${state.factory.fold( "Not computed" )( _.fold( identity, FactoryTable.render( state.inputs.bill, _ ) ) )}
            |
            |CUSTOM GROUPS
            |${state.customGroupSelection.customGroups
              .map { case ( recipe, n ) => s"$n: ${recipe.displayName}" }
              .mkString( "\n" )}
            |""".stripMargin
  )

  val inputsLens: Lens[PageState, SolverInputs]                     = lens[PageState].inputs
  val selectedInputTabLens: Lens[PageState, InputTab]               = lens[PageState].selectedInputTab
  val selectedOutputTabLens: Lens[PageState, OutputTab]             = lens[PageState].selectedOutputTab
  val factoryLens: Lens[PageState, Option[Either[String, Factory]]] = lens[PageState].factory
}
