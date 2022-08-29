package net.chwthewke.satisfactorytools
package web
package front

import cats.effect.IO
import outwatch._
import outwatch.dsl._
import cats.effect.SyncIO
import colibri.Subject

// Outwatch documentation:
// https://outwatch.github.io/docs/readme.html

object Main {
  def main( args: Array[String] ): Unit =
    Outwatch.renderInto[SyncIO]( "#app", app ).unsafeRunSync()

  def app: HtmlVNode = div(
    h1( "Hello World!" ),
    counter,
    inputField,
    App[IO]
  )

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
