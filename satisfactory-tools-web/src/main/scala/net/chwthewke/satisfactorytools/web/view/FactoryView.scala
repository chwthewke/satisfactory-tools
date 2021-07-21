package net.chwthewke.satisfactorytools
package web.view

import cats.Order
import cats.Order.catsKernelOrderingForOrder
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.order._
import cats.syntax.semigroup._
import cats.syntax.show._
import cats.syntax.traverse._
import enumeratum.Enum
import enumeratum.EnumEntry
import scala.collection.immutable.SortedMap
import scalatags.Text

import data.Countable
import data.Item
import model.Bill
import model.Machine
import model.Model
import model.Recipe
import prod.ClockedRecipe
import prod.Factory
import web.protocol.Forms
import web.state.PageState

object FactoryView {

  import Text.all._

  val Tolerance = 1e-4 // for display, bigger than in Calculator

  object Resources {
    def apply( factory: Factory ): Tag =
      fieldset(
        legend( "Raw resources" ),
        extractedResources( factory )
      )

    def extractedResources( factory: Factory ): Tag =
      table(
        extractedResourcesView( factory.extraction.map( _.productsPerMinute.head ).gather )
      )

    def extractedResourcesView( res: Vector[Countable[Double, Item]] ): Vector[Tag] =
      res
        .filter { case Countable( _, x ) => x.abs > Tolerance }
        .sortBy { case Countable( p, x ) => ( -x, p ) }
        .map {
          case Countable( p, x ) =>
            tr(
              numCell4( x ),
              td( p.displayName )
            )
        }
  }

  object Machines {

    def apply( recipes: Vector[ClockedRecipe] ): Tag =
      fieldset(
        legend( "Machines" ),
        machinesList( recipes.map( r => r.recipe.as( r.machine ) ) )
      )

    def machinesList( machines: Vector[Countable[Int, Machine]] ): Tag =
      table(
        machines.gather
          .sortBy( m => ( m.item.machineType, m.item.powerConsumption ) )
          .map(
            m =>
              tr(
                td( f"${m.amount}%3d", textAlign.right ),
                td( m.item.displayName )
              )
          )
      )
  }

  object Blocks {
    def apply( model: Model, state: PageState, factory: Factory ): Tag =
      fieldset(
        legend( "Manufacturing steps" ),
        recipeTable( model, state, factory, CustomGroupsRadios.Full )
      )

    def groupRadio(
        model: Model,
        state: PageState,
        recipe: Recipe,
        groupIndex: Int
    ): Tag =
      td(
        textAlign.center,
        input(
          `type` := "radio",
          name := Forms.outputGroup( model, recipe ),
          value := groupIndex,
          Option.when( state.customGroupSelection.customGroups.getOrElse( recipe, 0 ) == groupIndex )( checked )
        )
      )

    def recipeCell2Cols( recipe: Recipe ): Frag = {
      val recipeName  = recipe.displayName
      val altPrefix   = "Alternate: "
      val isAlternate = recipeName.startsWith( altPrefix )

      Seq[Frag](
        td(
          recipe.displayName.stripPrefix( altPrefix ),
          colspan := (if (isAlternate) 1 else 2),
          title := RecipeListView.describeRecipe( recipe ),
          textAlign.left
        ),
        Option.when( isAlternate )( td( "ALT", textAlign.right ) )
      )
    }

    def recipeRow(
        model: Model,
        state: PageState,
        block: ClockedRecipe,
        radios: CustomGroupsRadios
    ): Tag = {
      import block._

      def customGroupRadios: Frag = radios match {
        case CustomGroupsRadios.Empty       => None
        case CustomGroupsRadios.Placeholder => Some( td( colspan := (state.customGroupSelection.count + 1) ) )
        case CustomGroupsRadios.Full =>
          Some( 0.to( state.customGroupSelection.count ).map( groupRadio( model, state, block.recipe.item, _ ) ) )
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

    val headers =
      Vector(
        ( 1, "Nb.", textAlign.right ),
        ( 2, "Recipe", textAlign.left ),
        ( 2, "Machines", textAlign.left ),
        ( 3, "Rate", textAlign.left ),
        ( 2, "Power", textAlign.left )
      )

    def recipeTable(
        model: Model,
        state: PageState,
        factory: Factory,
        radios: CustomGroupsRadios
    ): Tag =
      table(
        thead(
          tr(
            Option.when( radios >= CustomGroupsRadios.Placeholder )(
              Seq[Frag](
                td( "-", textAlign.center ),
                1.to( state.customGroupSelection.count ).map( ix => td( ix.toString, textAlign.center ) )
              )
            ),
            headers.map { case ( w, h, al ) => th( colspan := w, h, al ) }
          )
        ),
        tbody(
          factory.extraction.map( recipeRow( model, state, _, radios = radios.min( CustomGroupsRadios.Placeholder ) ) ),
          factory.manufacturing.map( r => recipeRow( model, state, ClockedRecipe.roundUp( r ), radios ) ),
          tr(
            Option.when( radios >= CustomGroupsRadios.Placeholder )(
              td( colspan := (1 + state.customGroupSelection.count) )
            ),
            td( colspan := 8, textAlign.right, "Total Power" ),
            td( textAlign.right, factory.allRecipes.foldMap( _.power ).show ),
            td( textAlign.left, "MW" )
          )
        )
      )
  }

  object CustomGroup {
    def apply( model: Model, state: PageState, factory: Factory, group: Int ): Tag = {
      val subFactory = extractSubFactory( state, factory, group )

      fieldset(
        legend( s"Custom Group $group" ),
        Blocks.recipeTable( model, state, subFactory, CustomGroupsRadios.Empty ),
        Machines( subFactory.allRecipes ),
        fieldset(
          legend( "Inputs" ),
          table( Resources.extractedResourcesView( subFactory.extraInputs ) )
        ),
        fieldset(
          legend( "Outputs" ),
          table( Resources.extractedResourcesView( subFactory.extraOutputs ) )
        ),
        fieldset(
          legend( "Item I/O" ),
          Items.itemsInOutView( Bill.apply( Vector.empty ), subFactory, "OUTPUT" )
        )
      )
    }

    private def extractSubFactory( pageState: PageState, factory: Factory, group: Int ) = {
      val manufacturing = factory.manufacturing
        .filter( b => pageState.customGroupSelection.customGroups.get( b.item ).contains( group ) )

      val external = manufacturing
        .foldMap( _.flatTraverse( _.itemsPerMinute ) )
        .gather
        .filter( _.amount.abs > 1e-6 )

      val ( input, output ) = external.partition( _.amount < 0 )

      Factory(
        Vector.empty,
        manufacturing,
        Countable( input, -1d ).flatSequence,
        output
      )
    }
  }

  object Items {
    def apply( state: PageState, factory: Factory ): Tag =
      fieldset(
        legend( "Items I/O" ),
        itemsInOutView( state.inputs.bill, factory, "EXTRA" )
      )

    def itemInOutHalf( dir: String, rows: Map[String, Double] ): Vector[Tag] =
      rows.toVector.zipWithIndex.map {
        case ( ( dest, amount ), ix ) =>
          tr(
            numCell4( amount ),
            Option.when( ix == 0 )( td( rowspan := rows.size, verticalAlign.middle, dir ) ),
            td( dest )
          )
      }

    def itemInOut( item: Item, to: Map[String, Double], from: Map[String, Double] ): Tag =
      fieldset(
        legend( item.displayName ),
        table(
          tbody(
            itemInOutHalf( "from", from ),
            itemInOutHalf( "to", to )
          )
        )
      )

    def itemsInOut(
        bill: Bill,
        factory: Factory,
        extraKey: String
    ): SortedMap[Item, ( Map[String, Double], Map[String, Double] )] =
      factory.allRecipes.foldMap(
        block =>
          block.ingredientsPerMinute
            .foldMap {
              case Countable( item, amount ) =>
                SortedMap(
                  (
                    item,
                    (
                      Map( block.recipe.item.displayName -> amount ),
                      Map.empty[String, Double]
                    )
                  )
                )
            } |+|
            block.productsPerMinute
              .foldMap {
                case Countable( item, amount ) =>
                  SortedMap(
                    (
                      item,
                      (
                        Map.empty[String, Double],
                        Map( block.recipe.item.displayName -> amount )
                      )
                    )
                  )
              }
      ) |+|
        bill.items.foldMap {
          case Countable( item, amount ) =>
            SortedMap(
              (
                item,
                ( Map( ( "REQUESTED", amount ) ), Map.empty[String, Double] )
              )
            )
        } |+|
        factory.extraOutputs.foldMap {
          case Countable( item, amount ) =>
            SortedMap(
              (
                item,
                ( Map( ( extraKey, amount ) ), Map.empty[String, Double] )
              )
            )
        } |+|
        factory.extraInputs.foldMap {
          case Countable( item, amount ) =>
            SortedMap(
              (
                item,
                ( Map.empty[String, Double], Map( ( "INPUT", amount ) ) )
              )
            )
        }

    private def toleranceFilter[A]( m: Map[A, Double] ): Map[A, Double] =
      m.filter { case ( _, x ) => x.abs > Tolerance }

    def itemsInOutView( bill: Bill, factory: Factory, extraKey: String ): Tag =
      div(
        itemsInOut( bill, factory, extraKey ).toVector
          .map { case ( item, ( to, from ) ) => ( item, toleranceFilter( to ), toleranceFilter( from ) ) }
          .filterNot { case ( _, to, from ) => to.isEmpty && from.isEmpty }
          .map( (itemInOut _).tupled )
      )
  }

  def numCell3( value: Double ): Frag =
    td( f"$value%3.3f", title := value.toString, textAlign.right )

  def numCell4( value: Double ): Frag =
    td( f"$value%4.3f", title := value.toString, textAlign.right )

  sealed trait CustomGroupsRadios extends EnumEntry with Product
  object CustomGroupsRadios extends Enum[CustomGroupsRadios] {
    final case object Empty       extends CustomGroupsRadios
    final case object Placeholder extends CustomGroupsRadios
    final case object Full        extends CustomGroupsRadios

    override val values: Vector[CustomGroupsRadios] = findValues.toVector

    implicit val customGroupsRadiosOrder: Order[CustomGroupsRadios] = Order.by( indexOf )
  }

}
