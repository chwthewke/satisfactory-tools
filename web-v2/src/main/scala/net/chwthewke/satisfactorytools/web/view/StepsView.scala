package net.chwthewke.satisfactorytools
package web.view

import cats.Order
import cats.syntax.foldable._
import cats.syntax.order._
import cats.syntax.show._
import enumeratum.Enum
import enumeratum.EnumEntry
import scalatags.Text
import scalatags.Text.Tag

import model.GroupAssignments
import model.Recipe
import prod.ClockedRecipe
import prod.Factory
import web.forms._

object StepsView extends ( ( ( Factory, GroupAssignments ), Int ) => Tag ) {
  import Text.all._

  override def apply( steps: ( Factory, GroupAssignments ), groupCount: Int ): Tag =
    fieldset(
      legend( "Manufacturing steps" ),
      recipeTable( steps._1, steps._2, groupCount, CustomGroupsRadios.Full )
    )

  def recipeTable(
      factory: Factory,
      groups: GroupAssignments,
      groupCount: Int,
      radios: CustomGroupsRadios
  ): Tag = {
    def groupHeaders: Frag = radios match {
      case CustomGroupsRadios.Empty   => Seq.empty[Frag]
      case CustomGroupsRadios.Sorting => Seq( th( colspan := 2 ) )
      case CustomGroupsRadios.Placeholder | CustomGroupsRadios.Full =>
        Seq[Frag](
          th( "0", textAlign.center ),
          1.to( groupCount ).map( ix => th( ix.toString, textAlign.center ) )
        )
    }

    table(
      `class` := "table is-striped",
      thead(
        tr(
          groupHeaders,
          headers.map { case ( w, h, al ) => th( colspan := w, h, al ) }
        )
      ),
      tbody(
        factory.extraction
          .map(
            recipe =>
              recipeRow(
                recipe,
                RowIndex.zero,
                groups,
                groupCount,
                radios = radios.min( CustomGroupsRadios.Placeholder )
              )
          ),
        factory.manufacturing.zipWithIndex.map {
          case ( r, ix ) =>
            recipeRow(
              ClockedRecipe.roundUp( r ),
              RowIndex( ix, factory.manufacturing.size ),
              groups,
              groupCount,
              radios
            )
        },
        tr(
          td( colspan := (if (radios >= CustomGroupsRadios.Placeholder) groupCount else 1) ),
          td( colspan := 9, textAlign.right, "Total Power" ),
          td( textAlign.right, factory.allRecipes.foldMap( _.power ).show ),
          td( textAlign.left, "MW" )
        )
      )
    )
  }

  def recipeRow(
      block: ClockedRecipe,
      rowIndex: RowIndex,
      groups: GroupAssignments,
      groupCount: Int,
      radios: CustomGroupsRadios
  ): Tag = {
    import block._

    def customGroupRadios: Frag = radios match {
      case CustomGroupsRadios.Empty       => None
      case CustomGroupsRadios.Sorting     => Some( groupOrder( rowIndex ) )
      case CustomGroupsRadios.Placeholder => Some( td( colspan := (groupCount + 1) ) )
      case CustomGroupsRadios.Full =>
        Some( 0.to( groupCount ).map( groupRadio( block.recipe.item, groups, _ ) ) )
    }

    tr(
      customGroupRadios,
      numCell4( itemAmount ),
      recipeCell2Cols( recipe.item ),
      td( f"$machineCount%3d", textAlign.right ),
      td( machine.displayName, textAlign.left ),
      numCell3( itemAmountPerUnit ),
      td( " / unit @ ", textAlign.center ),
      td( f"${clockSpeedMillionth / 10000}%3d.${clockSpeedMillionth % 10000}%04d %%", textAlign.left ),
      td( power.show, textAlign.right ),
      td( "MW", textAlign.left )
    )
  }

  def recipeCell2Cols( recipe: Recipe ): Frag = {
    val recipeName  = recipe.displayName
    val altPrefix   = "Alternate: "
    val isAlternate = recipeName.startsWith( altPrefix )

    Seq[Frag](
      td(
        recipe.displayName.stripPrefix( altPrefix ),
        colspan := (if (isAlternate) 1 else 2),
        title := RecipesView.describeRecipe( recipe ),
        textAlign.left
      ),
      Option.when( isAlternate )( td( "ALT", textAlign.right ) )
    )
  }

  def groupRadio(
      recipe: Recipe,
      groups: GroupAssignments,
      groupIndex: Int
  ): Tag =
    td(
      textAlign.center,
      input(
        `class` := "radio",
        `type` := "radio",
        name := Keys.outputGroup( recipe.className ),
        value := groupIndex,
        Option.when( groups.getOrElse( recipe.className, 0 ) == groupIndex )( checked )
      )
    )

  def groupOrder(
      rowIndex: RowIndex
  ): Frag = Seq(
    td(
      button(
        formaction := s"${Actions.outputGroupOrder}/${rowIndex.index - 1}",
        Option.when( !rowIndex.canMoveUp )( disabled ),
        "\u25B2"
      )
    ),
    td(
      button(
        formaction := s"${Actions.outputGroupOrder}/${rowIndex.index}",
        Option.when( !rowIndex.canMoveDown )( disabled ),
        "\u25BC"
      )
    )
  )

  val headers: Vector[( Int, String, Modifier )] =
    Vector(
      ( 1, "Nb.", textAlign.right ),
      ( 2, "Recipe", textAlign.left ),
      ( 2, "Machines", textAlign.left ),
      ( 3, "Rate", textAlign.left ),
      ( 2, "Power", textAlign.left )
    )

  final case class RowIndex( index: Int, total: Int ) {
    def canMoveUp: Boolean   = index > 0
    def canMoveDown: Boolean = index + 1 < total
  }

  object RowIndex {
    val zero: RowIndex = RowIndex( 0, 0 )
  }

  sealed trait CustomGroupsRadios extends EnumEntry with Product
  object CustomGroupsRadios extends Enum[CustomGroupsRadios] {
    final case object Empty       extends CustomGroupsRadios
    final case object Sorting     extends CustomGroupsRadios
    final case object Placeholder extends CustomGroupsRadios
    final case object Full        extends CustomGroupsRadios

    override val values: Vector[CustomGroupsRadios] = findValues.toVector

    implicit val customGroupsRadiosOrder: Order[CustomGroupsRadios] = Order.by( indexOf )
  }

}
