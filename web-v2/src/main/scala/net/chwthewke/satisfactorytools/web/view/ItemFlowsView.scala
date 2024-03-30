package net.chwthewke.satisfactorytools
package web.view

import scalatags.Text.all.{data => _, _}

import data.Item
import model.Options
import prod.planning.FactoryPlan

object ItemFlowsView extends ( ( ( FactoryPlan, Options, Option[Item] ), Int ) => Tag ) {

  override def apply( itemFlowsData: ( FactoryPlan, Options, Option[Item] ), groupCount: Int ): Tag =
    ( of _ ).tupled( itemFlowsData )

  private def of( plan: FactoryPlan, options: Options, itemOpt: Option[Item] ): Tag =
    div(
      showItems( plan, options )
    )

  private def showItems( plan: FactoryPlan, options: Options ): Frag =
    plan.itemFlows.map {
      case ( item, flow ) => showItem( item, flow, options )
    }.toSeq

  private def showItem( item: Item, itemFlow: FactoryPlan.ItemFlow#WithTransports, options: Options ): Frag = ???

}
