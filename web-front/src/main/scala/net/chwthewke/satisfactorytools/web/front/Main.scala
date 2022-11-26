package net.chwthewke.satisfactorytools
package web
package front

import cats.effect.IO
import cats.effect.SyncIO
import colibri.Observable
import colibri.Subject
import colibri.ext.fs2._
import fs2.Stream
import org.http4s.client.Client
import outwatch._
import outwatch.dsl._

import web.api.DefsApi
import web.front.http.DefsClient
import web.front.view.InitView
import web.front.vm.InitVm

// Outwatch documentation:
// https://outwatch.github.io/docs/readme.html

object Main {
  def main( args: Array[String] ): Unit =
    Outwatch.renderInto[SyncIO]( "#app", app ).unsafeRunSync()

  def app: HtmlVNode = div(
    h1( "Hello World2!" ),
    Observable.lift( loadModel ).map( InitView( _ ) )
  )

  val client: Client[IO] = Resources.httpClientIn[IO, SyncIO].unsafeRunSync()

  val defsClient: DefsApi[IO] = DefsClient[IO]( client )

  val loadModel: Stream[IO, InitVm] =
    Stream
      .eval(
        defsClient.getVersions
          .flatMap( versions => defsClient.getModel( versions.maxBy( _._2.version )._1 ).value )
          .map[InitVm => InitVm]( modelOpt => (vm: InitVm) => vm.modelReceived( modelOpt ) )
      )
      .scan[InitVm]( InitVm.init )( ( vm, f ) => f( vm ) )

  def counter: SyncIO[HtmlVNode] = SyncIO {
    // https://outwatch.github.io/docs/readme.html#example-counter
    val number = Subject.behavior( 0 )
    div(
      button(
        `class` := "button is-small is-primary",
        "+",
        onClick( number.map( _ + 1 ) ) --> number,
        marginRight := "10px"
      ),
      number
    )
  }

  def inputField: SyncIO[HtmlVNode] = SyncIO {
    // https://outwatch.github.io/docs/readme.html#example-input-field
    val text = Subject.behavior( "" )
    div(
      div(
        `class` := "field has-addons",
        div(
          `class` := "control",
          input(
            `class` := "input",
            `type` := "text",
            value <-- text,
            onInput.value --> text
          )
        ),
        button(
          `class` := "button is-primary",
          "clear",
          onClick.as( "" ) --> text
        )
      ),
      div( "text: ", text ),
      div( "length: ", text.map( _.length ) )
    )
  }

}
