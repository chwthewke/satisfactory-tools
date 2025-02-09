package net.chwthewke.satisfactorytools
package web
package view

object Table {

  import scalatags.Text.all._

  def groupedSortableCell(
      startsGroup: Boolean,
      rowIndex: RowIndex,
      upAction: String,
      downAction: String,
      groupAction: String,
      buttonsOnRight: Boolean = false
  )( contents: Modifier* ): Modifier = {

    Seq[Modifier](
      td(
        position.relative,
        Option.when( startsGroup && rowIndex.index > 0 )(
          borderTop := "3px solid white"
        ),
        Option.when( rowIndex.index > 0 )(
          button(
            formaction := s"$groupAction/${rowIndex.index}",
            if (startsGroup) "x" else "\u2014",
            position.absolute,
            top := "-0.9em",
            if (buttonsOnRight) right := "1.1em" else left := "-1.1em"
          )
        ),
        Option.when( buttonsOnRight )( contents ),
        button(
          formaction := s"$upAction/${rowIndex.index}",
          Option.when( !rowIndex.canMoveUp )( disabled ),
          "\u25B2"
        ),
        button(
          formaction := s"$downAction/${rowIndex.index}",
          Option.when( !rowIndex.canMoveDown )( disabled ),
          "\u25BC"
        ),
        Option.when( !buttonsOnRight )( contents )
      )
    )
  }

}
