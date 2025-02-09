package net.chwthewke.satisfactorytools
package web.view

import cats.syntax.all._
import scalatags.Text
import scalatags.Text.Tag

import data.ClassName
import model.GroupAssignment
import model.GroupAssignments
import model.Power
import model.Recipe
import prod.ClockedRecipe
import prod.Factory
import protocol.OutputTab
import web.forms._

case class StepsView( editGroups: Boolean ) extends ( ( ( Factory, GroupAssignments[ClassName] ), Int ) => Tag ) {
  import Text.all._

  import StepsView._

  override def apply( steps: ( Factory, GroupAssignments[ClassName] ), groupCount: Int ): Tag =
    fieldset(
      legend(
        "Manufacturing steps",
        raw( "&nbsp;" ),
        button(
          formaction := s"output/${Actions.output( OutputTab.Steps( !editGroups ) )}",
          if (editGroups) "Done editing groups" else "Edit groups"
        )
      ),
      recipeTable( steps._1, if (editGroups) CustomGroupsRadios.Full( steps._2 ) else CustomGroupsRadios.Empty )
    )
}

object StepsView {
  import Text.all._

  def recipeTable(
      factory: Factory,
      radios: CustomGroupsRadios
  ): Tag = {

    val ( groupHeaders: Seq[Frag], groupWidth: Int ) = radios match {
      case CustomGroupsRadios.Empty        => ( Seq.empty[Frag], 0 )
      case CustomGroupsRadios.Group( _ )   => ( Seq.empty[Frag], 0 )
      case CustomGroupsRadios.Sorting( _ ) => ( Seq( th( colspan := 2 ) ), 2 )
      case CustomGroupsRadios.Full( groups ) =>
        (
          Seq[Frag](
            th( "0", textAlign.center ),
            1.to( groups.count ).map( ix => th( ix.toString, textAlign.center ) )
          ),
          groups.count + 1
        )
    }

    table(
      position.relative,
      left    := "1em",
      `class` := "table is-striped",
      borderCollapse.collapse,
      thead(
        tr(
          groupHeaders,
          headers.map { case ( w, h, al ) => th( colspan := w, h, al ) }
        )
      ),
      tbody(
        factory.extraction
          .map( recipe =>
            recipeRow(
              recipe,
              RowIndex.zero,
              radios = CustomGroupsRadios.Placeholder( groupWidth )
            )
          ),
        factory.manufacturing.zipWithIndex.map {
          case ( r, ix ) =>
            recipeRow(
              ClockedRecipe.roundUp( r ),
              RowIndex( ix, factory.manufacturing.size ),
              radios
            )
        },
        tr(
          Option.when( groupWidth > 0 )( td( colspan := groupWidth ) ),
          td( colspan := 8, textAlign.right, "Total Power" ),
          td( textAlign.right, powerCell( factory.allRecipes.foldMap( _.power ) ) ),
          td( textAlign.left, "MW" )
        )
      )
    )

  }

  def recipeRow(
      block: ClockedRecipe,
      rowIndex: RowIndex,
      radios: CustomGroupsRadios.Internal
  ): Tag = {
    import block._

    def customGroupRadios: Modifier = radios match {
      case CustomGroupsRadios.Empty                     => none[Modifier]
      case CustomGroupsRadios.Group( group )            => Some( groupSection( rowIndex, group ) )
      case CustomGroupsRadios.Sorting( group )          => Some( groupOrder( rowIndex, group ) )
      case CustomGroupsRadios.Placeholder( groupWidth ) => Option.when( groupWidth > 0 )( td( colspan := groupWidth ) )
      case CustomGroupsRadios.Full( groups ) =>
        Some( 0.to( groups.count ).map( groupRadio( recipe.item, groups, _ ) ) )
    }

    tr(
      customGroupRadios,
      numCell4( mainProductAmount ),
      recipeCell2Cols( recipe.item ),
      td( f"$machineCount%3d", textAlign.right ),
      td( machine.displayName, textAlign.left ),
      numCell3( mainProductAmountPerUnit ),
      td( " / unit @ ", textAlign.center ),
      td( f"${clockSpeedMillionth / 10000}%3d.${clockSpeedMillionth % 10000}%04d %%", textAlign.left ),
      td( powerCell( power ), textAlign.right ),
      td( "MW", textAlign.left )
    )
  }

  def powerCell( power: Power ): Tag =
    power match {
      case Power.Fixed( value ) =>
        span( f"$value%.2f", title := value.toString )
      case Power.Variable( min, max ) =>
        span(
          f"${power.average}%.2f",
          title :=
            s"""${f"$min%.2f-$max%.2f"}
               |min: $min
               |max: $max
               |""".stripMargin
        )
    }

  def recipeCell2Cols( recipe: Recipe ): Frag = {
    val recipeName  = recipe.displayName
    val altPrefix   = "Alternate: "
    val isAlternate = recipeName.startsWith( altPrefix )

    Seq[Frag](
      td(
        recipe.displayName.stripPrefix( altPrefix ),
        colspan := ( if (isAlternate) 1 else 2 ),
        title   := RecipesView.describeRecipe( recipe ),
        textAlign.left
      ),
      Option.when( isAlternate )( td( "ALT", textAlign.right ) )
    )
  }

  def groupRadio(
      recipe: Recipe,
      groups: GroupAssignments[ClassName],
      groupIndex: Int
  ): Tag =
    td(
      textAlign.center,
      input(
        `class` := "radio",
        `type`  := "radio",
        name    := Keys.outputGroup( recipe.className ),
        value   := groupIndex,
        Option.when( groups.getOrElse( recipe.className, 0 ) == groupIndex )( checked )
      )
    )

  private def groupSection( rowIndex: RowIndex, sectionsBefore: Set[Int] ): Modifier =
    Option.when( sectionsBefore( rowIndex.index ) )( borderTop := "3px solid white" )

  private def locateSections( group: GroupAssignment[ClassName] ): Set[Int] =
    group.sections
      .foldLeft( ( 0, Set.empty[Int] ) ) {
        case ( ( counter, acc ), section ) =>
          ( counter + section.length, acc + counter )
      }
      ._2
      .excl( 0 )

  def groupSection(
      rowIndex: RowIndex,
      group: GroupAssignment[ClassName]
  ): Modifier = groupSection( rowIndex, locateSections( group ) )

  def groupOrder(
      rowIndex: RowIndex,
      group: GroupAssignment[ClassName]
  ): Modifier = {
    val sectionsBefore: Set[Int] = locateSections( group )

    Seq[Modifier](
      position.relative,
      groupSection( rowIndex, sectionsBefore ),
      td(
        Option.when( rowIndex.index > 0 )(
          button(
            formaction := s"${Actions.outputGroupSection}/${rowIndex.index}",
            if (sectionsBefore( rowIndex.index )) "x" else "\u2014",
            position.absolute,
            top  := "-0.9em",
            left := "-1.1em"
          )
        ),
        button(
          formaction := s"${Actions.outputGroupMoveUp}/${rowIndex.index}",
          Option.when( !rowIndex.canMoveUp )( disabled ),
          "\u25B2"
        )
      ),
      td(
        button(
          formaction := s"${Actions.outputGroupMoveDown}/${rowIndex.index}",
          Option.when( !rowIndex.canMoveDown )( disabled ),
          "\u25BC"
        )
      )
    )
  }

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

  sealed trait CustomGroupsRadios extends CustomGroupsRadios.Internal
  object CustomGroupsRadios {
    sealed trait Internal extends Product with Serializable

    final case object Empty                                       extends CustomGroupsRadios
    final case class Group( group: GroupAssignment[ClassName] )   extends CustomGroupsRadios
    final case class Sorting( group: GroupAssignment[ClassName] ) extends CustomGroupsRadios
    final case class Placeholder( width: Int )                    extends CustomGroupsRadios.Internal
    final case class Full( groups: GroupAssignments[ClassName] )  extends CustomGroupsRadios
  }
}
