package net.chwthewke.satisfactorytools
package web.view

import scalatags.Text.all._

object ShutdownView {
  def apply(): Tag = {
    page(
      "Shutting down...",
      div(
        classes( "block" ),
        h1(
          classes( "title" ),
          "Shutdown requested"
        )
      )
    )
  }
}
