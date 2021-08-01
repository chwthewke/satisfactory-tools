package net.chwthewke.satisfactorytools
package web

import scalatags.Text
import scalatags.Text.all._
import scalatags.Text.Tag

package object view {

  val AmountTolerance: Double = 1e-4

  val pageStyle: Tag = Text.tags2.style(
    // language=CSS
    """#main {
      |  display: flex;
      |  flex-flow: row;
      |}
      |
      |#input {
      |  flex: auto;
      |}
      |
      |#output {
      |  flex: auto;
      |}
      |
      |""".stripMargin
  )

  def numCell3( value: Double ): Frag =
    td( f"$value%3.3f", title := value.toString, textAlign.right )

  def numCell4( value: Double ): Frag =
    td( f"$value%4.3f", title := value.toString, textAlign.right )

}
