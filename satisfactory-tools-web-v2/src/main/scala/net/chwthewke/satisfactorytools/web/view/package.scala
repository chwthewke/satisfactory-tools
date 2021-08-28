package net.chwthewke.satisfactorytools
package web

import scalatags.Text.all._
import scalatags.Text.Tag

package object view {

  val AmountTolerance: Double = 1e-4

  def page( pageTitle: String, contents: Tag ): Tag =
    html(
      head(
        title := pageTitle,
        link( rel := "stylesheet", href := "/style.css" )
      ),
      body(
        contents
      )
    )

  def numCell3( value: Double ): Frag =
    td( f"$value%3.3f", title := value.toString, textAlign.right )

  def numCell4( value: Double ): Frag =
    td( f"$value%4.3f", title := value.toString, textAlign.right )

}
