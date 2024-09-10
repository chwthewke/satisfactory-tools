package net.chwthewke.satisfactorytools
package web.view

import scalatags.Text
import scalatags.Text.Tag

import data.Countable
import model.Machine

object MachinesView extends ( ( Vector[Countable[Int, Machine]], Int ) => Tag ) {

  import Text.all._

  override def apply( machines: Vector[Countable[Int, Machine]], groupCount: Int ): Tag =
    fieldset(
      legend( "Machines" ),
      table(
        machines.gather
          .sortBy( m => ( m.item.machineType, m.item.powerConsumption ) )
          .map( m =>
            tr(
              td( f"${m.amount}%3d", textAlign.right ),
              td( m.item.displayName )
            )
          )
      )
    )
}
