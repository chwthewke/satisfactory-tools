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

import data.ClassName
import model.Recipe
import prod.ClockedRecipe
import prod.Factory
import web.forms._

object StepsView extends ( ( ( Factory, Map[ClassName, Int] ), Int ) => Tag ) {
  import Text.all._

  override def apply( steps: ( Factory, Map[ClassName, Int] ), groupCount: Int ): Tag =
    fieldset(
      legend( "Manufacturing steps" ),
      recipeTable( steps._1, steps._2, groupCount, CustomGroupsRadios.Full )
    )

  def recipeTable(
      factory: Factory,
      groups: Map[ClassName, Int],
      groupCount: Int,
      radios: CustomGroupsRadios
  ): Tag =
    table(
      thead(
        tr(
          Option.when( radios >= CustomGroupsRadios.Placeholder )(
            Seq[Frag](
              td( "-", textAlign.center ),
              1.to( groupCount ).map( ix => td( ix.toString, textAlign.center ) )
            )
          ),
          headers.map { case ( w, h, al ) => th( colspan := w, h, al ) }
        )
      ),
      tbody(
        factory.extraction
          .map( recipeRow( _, groups, groupCount, radios = radios.min( CustomGroupsRadios.Placeholder ) ) ),
        factory.manufacturing.map( r => recipeRow( ClockedRecipe.roundUp( r ), groups, groupCount, radios ) ),
        tr(
          Option.when( radios >= CustomGroupsRadios.Placeholder )(
            td( colspan := (1 + groupCount) )
          ),
          td( colspan := 8, textAlign.right, "Total Power" ),
          td( textAlign.right, factory.allRecipes.foldMap( _.power ).show ),
          td( textAlign.left, "MW" )
        )
      )
    )

  def recipeRow(
      block: ClockedRecipe,
      groups: Map[ClassName, Int],
      groupCount: Int,
      radios: CustomGroupsRadios
  ): Tag = {
    import block._

    def customGroupRadios: Frag = radios match {
      case CustomGroupsRadios.Empty       => None
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
      groups: Map[ClassName, Int],
      groupIndex: Int
  ): Tag =
    td(
      textAlign.center,
      input(
        `type` := "radio",
        name := Keys.outputGroup( recipe ),
        value := groupIndex,
        Option.when( groups.getOrElse( recipe.className, 0 ) == groupIndex )( checked )
      )
    )

  val headers =
    Vector(
      ( 1, "Nb.", textAlign.right ),
      ( 2, "Recipe", textAlign.left ),
      ( 2, "Machines", textAlign.left ),
      ( 3, "Rate", textAlign.left ),
      ( 2, "Power", textAlign.left )
    )

  sealed trait CustomGroupsRadios extends EnumEntry with Product
  object CustomGroupsRadios extends Enum[CustomGroupsRadios] {
    final case object Empty       extends CustomGroupsRadios
    final case object Placeholder extends CustomGroupsRadios
    final case object Full        extends CustomGroupsRadios

    override val values: Vector[CustomGroupsRadios] = findValues.toVector

    implicit val customGroupsRadiosOrder: Order[CustomGroupsRadios] = Order.by( indexOf )
  }

}
