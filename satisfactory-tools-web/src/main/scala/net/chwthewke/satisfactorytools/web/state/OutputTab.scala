package net.chwthewke.satisfactorytools
package web.state

import cats.Show
import org.http4s.FormDataDecoder
import scalatags.Text.Tag
import scodec.Codec
import scodec.codecs

import model.Model
import prod.Factory
import web.protocol.Forms
import web.view.FactoryView

abstract class OutputTab( val id: String, private val index: Int ) extends Product {
  def view( model: Model, state: PageState, solution: Factory ): Tag

  def customGroupsFormDataDecoder( model: Model ): Option[FormDataDecoder[CustomGroupSelection]] = None
}

object OutputTab {

  final case object BlocksTab extends OutputTab( "steps", 253 ) {
    override def view( model: Model, state: PageState, solution: Factory ): Tag =
      FactoryView.Blocks( model, state, solution )

    override def customGroupsFormDataDecoder( model: Model ): Option[FormDataDecoder[CustomGroupSelection]] =
      Some( Forms.customGroups( model ) )
  }

  final case object ResourcesTab extends OutputTab( "resources", 254 ) {
    override def view( model: Model, state: PageState, solution: Factory ): Tag =
      FactoryView.Resources( solution )
  }

  final case object MachinesTab extends OutputTab( "machines", 252 ) {
    override def view( model: Model, state: PageState, solution: Factory ): Tag =
      FactoryView.Machines( solution.allRecipes )
  }

  final case object ItemsTab extends OutputTab( "items", 255 ) {
    override def view( model: Model, state: PageState, solution: Factory ): Tag =
      FactoryView.Items( state, solution )
  }

  final case class CustomGroup( ix: Int ) extends OutputTab( s"group$ix", ix ) {
    override def view( model: Model, state: PageState, solution: Factory ): Tag =
      FactoryView.CustomGroup( model, state, solution, ix )
  }

  object CustomGroup {
    import atto._
    import Atto._
    def parse( s: String ): Option[CustomGroup] =
      (string( "group" ) ~> int).map( CustomGroup( _ ) ).parseOnly( s ).option
  }

  def withId( s: String ): Option[OutputTab] =
    s match {
      case BlocksTab.id    => Some( BlocksTab )
      case ResourcesTab.id => Some( ResourcesTab )
      case ItemsTab.id     => Some( ItemsTab )
      case MachinesTab.id  => Some( MachinesTab )
      case _               => CustomGroup.parse( s )
    }

  private def fromInt( n: Int ): OutputTab = n match {
    case 255 => ItemsTab
    case 254 => ResourcesTab
    case 253 => BlocksTab
    case 252 => MachinesTab
    case x   => CustomGroup( x )
  }

  implicit val outputTabCodec: Codec[OutputTab] = codecs.uint8.xmap( fromInt, _.index )

  implicit val outputTabShow: Show[OutputTab] = Show.fromToString
}
