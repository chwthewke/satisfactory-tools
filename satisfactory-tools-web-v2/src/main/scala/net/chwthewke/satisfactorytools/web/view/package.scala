package net.chwthewke.satisfactorytools
package web

import scalatags.Text
import scalatags.Text.Tag
import scalatags.Text.all._

package object view {

  val AmountTolerance: Double = 1e-4

  val pageStyle: Tag = Text.tags2.style(
    // language=CSS
    """#library {
      |    font-size: 1.5rem;
      |}
      |
      |#library td, #library th{
      |    padding: 0.5em;
      |}
      |
      |#header > *:not(:first-child):not(:last-child) {
      |    margin: 5px;
      |}
      |
      |#header > *:first-child:not(:last-child) {
      |    margin: 5px 5px 5px 0;
      |}
      |
      |#header > *:not(:first-child):last-child {
      |    margin: 5px 0 5px 5px;
      |}
      |
      |#main {
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

  def page( pageTitle: String, contents: Tag ): Tag =
    html(
      head( title := pageTitle, pageStyle ),
      body( contents )
    )

  def numCell3( value: Double ): Frag =
    td( f"$value%3.3f", title := value.toString, textAlign.right )

  def numCell4( value: Double ): Frag =
    td( f"$value%4.3f", title := value.toString, textAlign.right )

}
