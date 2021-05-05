package net.chwthewke.satisfactorytools
package web.view

import cats.Order
import cats.Order.catsKernelOrderingForOrder
import cats.syntax.foldable._
import cats.syntax.flatMap._
import cats.syntax.order._
import cats.syntax.semigroup._
import cats.syntax.traverse._
import enumeratum.Enum
import enumeratum.EnumEntry
import scala.collection.immutable.SortedMap
import scalatags.Text
import scalatags.Text.all._

import model.Bill
import model.Countable
import model.Item
import model.Machine
import model.Model
import model.Recipe
import prod.Factory
import prod.FactoryBlock
import web.protocol.Forms
import web.state.PageState

object FactoryView {

  val customGroups: Int = 5

  object Resources {
    def apply( factory: Factory ): Text.TypedTag[String] =
      fieldset(
        legend( "Raw resources" ),
        extractedResources( factory )
      )

    def extractedResources( factory: Factory ): Text.TypedTag[String] =
      table(
        extractedResourcesView(
          factory.extraction.map( _.recipe.flatMap( _.productsPerMinute.head ) ).gather
        )
      )

    def extractedResourcesView( res: Vector[Countable[Double, Item]] ): Vector[Text.TypedTag[String]] =
      res
        .sortBy { case Countable( p, x ) => ( -x, p ) }
        .map {
          case Countable( p, x ) =>
            tr(
              td( textAlign := "right", f"$x%.3f" ),
              td( p.displayName )
            )
        }
  }

  object Blocks {
    def apply( model: Model, state: PageState, factory: Factory ): Text.TypedTag[String] =
      fieldset(
        legend( "Manufacturing steps" ),
        recipeTable( model, state, factory, CustomGroupsRadios.Full )
      )

    def groupRadio(
        model: Model,
        state: PageState,
        recipe: Recipe[Machine, Item],
        groupIndex: Int
    ): Text.TypedTag[String] =
      td(
        textAlign.center,
        input(
          `type` := "radio",
          name := Forms.outputGroup( model, recipe ),
          value := groupIndex,
          Option.when( state.customGroupSelection.customGroups.getOrElse( recipe, 0 ) == groupIndex )( checked )
        )
      )

    def recipeRow(
        model: Model,
        state: PageState,
        block: FactoryBlock,
        radios: CustomGroupsRadios
    ): Text.TypedTag[String] = {
      import block._

      val recipeName  = recipe.item.displayName
      val altPrefix   = "Alternate: "
      val isAlternate = recipeName.startsWith( altPrefix )

      def customGroupRadios: Frag = radios match {
        case CustomGroupsRadios.Empty       => None
        case CustomGroupsRadios.Placeholder => Some( td( colspan := (customGroups + 1) ) )
        case CustomGroupsRadios.Full =>
          Some( 0.to( customGroups ).map( groupRadio( model, state, block.recipe.item, _ ) ) )
      }

      tr(
        customGroupRadios,
        td( f"$itemAmount%4.3f", textAlign.right ),
        td(
          recipe.item.displayName.stripPrefix( altPrefix ),
          colspan := (if (isAlternate) 1 else 2),
          title := RecipeListView.describeRecipe( recipe.item ),
          textAlign.left
        ),
        Option.when( isAlternate )( td( "ALT", textAlign.right ) ),
        td( f"$machineCount%3d", textAlign.right ),
        td( machine.displayName, textAlign.left ),
        td( f"$itemAmountPerUnit%3.3f / unit", textAlign.right ),
        td( " @ ", textAlign.center ),
        td( f"${clockSpeedMillionth / 10000}%3d.${clockSpeedMillionth % 10000}%04d %%", textAlign.left ),
        td( f"$power%4.2f", textAlign.right ),
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
    ): Text.TypedTag[String] =
      table(
        thead(
          tr(
            Option.when( radios >= CustomGroupsRadios.Placeholder )(
              Seq[Frag](
                td( "-", textAlign.center ),
                1.to( customGroups ).map( ix => td( ix.toString, textAlign.center ) )
              )
            ),
            headers.map { case ( w, h, al ) => th( colspan := w, h, al ) }
          )
        ),
        tbody(
          factory.extraction.map( recipeRow( model, state, _, radios = radios.min( CustomGroupsRadios.Placeholder ) ) ),
          factory.manufacturing.map( recipeRow( model, state, _, radios ) ),
          tr(
            Option.when( radios >= CustomGroupsRadios.Placeholder )(
              td( colspan := (customGroups + 1) )
            ),
            td( colspan := 8, textAlign.right, "Total Power" ),
            td( textAlign.right, f"${(factory.extraction ++ factory.manufacturing).foldMap( _.power )}%4.2f" ),
            td( textAlign.left, "MW" )
          )
        )
      )
  }

  object CustomGroup {
    def apply( model: Model, state: PageState, factory: Factory, group: Int ): Text.TypedTag[String] = {
      val subFactory = extractSubFactory( model, state, factory, group )

      fieldset(
        legend( s"Custom Group $group" ),
        Blocks.recipeTable( model, state, subFactory, CustomGroupsRadios.Empty ),
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

    private def extractSubFactory( model: Model, pageState: PageState, factory: Factory, group: Int ) = {
      val manufacturing = factory.manufacturing
        .filter( b => pageState.customGroupSelection.customGroups.get( b.recipe.item ).contains( group ) )

      val external = manufacturing
        .foldMap( _.recipe.flatTraverse( _.itemsPerMinute ) )
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
    def apply( state: PageState, factory: Factory ): Text.TypedTag[String] =
      fieldset(
        legend( "Items I/O" ),
        itemsInOutView( state.inputs.bill, factory, "EXTRA" )
      )

    def itemInOutHalf( dir: String, rows: Map[String, Double] ): Vector[Text.TypedTag[String]] =
      rows.toVector.zipWithIndex.map {
        case ( ( dest, amount ), ix ) =>
          tr(
            td( f"$amount%4.3f" ),
            Option.when( ix == 0 )( td( rowspan := rows.size, verticalAlign := "middle", dir ) ),
            td( dest )
          )
      }

    def itemInOut( item: Item, to: Map[String, Double], from: Map[String, Double] ): Text.TypedTag[String] =
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
      (factory.extraction ++ factory.manufacturing).foldMap(
        block =>
          block.recipe.item.ingredientsPerMinute
            .foldMap {
              case Countable( item, amount ) =>
                SortedMap(
                  (
                    item,
                    (
                      Map( block.recipe.item.displayName -> block.recipe.amount * amount ),
                      Map.empty[String, Double]
                    )
                  )
                )
            } |+|
            block.recipe.item.productsPerMinute
              .foldMap {
                case Countable( item, amount ) =>
                  SortedMap(
                    (
                      item,
                      (
                        Map.empty[String, Double],
                        Map( block.recipe.item.displayName -> block.recipe.amount * amount )
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

    def itemsInOutView( bill: Bill, factory: Factory, extraKey: String ): Text.TypedTag[String] =
      div(
        itemsInOut( bill, factory, extraKey ).toVector.map {
          case ( item, ( to, from ) ) => itemInOut( item, to, from )
        }
      )
  }

  sealed trait CustomGroupsRadios extends EnumEntry with Product
  object CustomGroupsRadios extends Enum[CustomGroupsRadios] {
    final case object Empty       extends CustomGroupsRadios
    final case object Placeholder extends CustomGroupsRadios
    final case object Full        extends CustomGroupsRadios

    override val values: Vector[CustomGroupsRadios] = findValues.toVector

    implicit val customGroupsRadiosOrder: Order[CustomGroupsRadios] = Order.by( indexOf )
  }

}
