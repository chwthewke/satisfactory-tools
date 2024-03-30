package net.chwthewke.satisfactorytools
package web

import scalatags.Text._
import scalatags.Text.all._
import scalatags.text.Builder

package object view {

  val AmountTolerance: Double = 1e-4

  def page( pageTitle: String, contents: Tag ): Tag =
    html(
      head(
        tags2.title( pageTitle ),
        meta( name := "viewport", content := "width=device-width, initial-scale=1" ),
        link( rel := "stylesheet", href := "/bulma.css" )
      ),
      body( contents )
    )

  def numCell3( value: Double ): Tag =
    td( f"$value%3.3f", title := value.toString, textAlign.right )

  def numCell4( value: Double ): Tag =
    td( f"$value%4.3f", title := value.toString, textAlign.right )

  object classes {
    def apply( names: String* ): Modifier =
      clsModifier( names )

    def +=( name: String ): Modifier =
      apply( name )

    def ++=( names: Seq[String] ): Modifier =
      apply( names: _* )
  }

  private def clsModifier( classNames: Seq[String] ): Modifier = new Modifier {
    override def applyTo( t: Builder ): Unit = {
      classNames.foreach( n => t.appendAttr( "class", Builder.GenericAttrValueSource( n ) ) )
    }
  }

}
