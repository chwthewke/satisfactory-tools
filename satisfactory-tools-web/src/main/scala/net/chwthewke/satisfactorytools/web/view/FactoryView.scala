package net.chwthewke.satisfactorytools
package web.view

import cats.Order.catsKernelOrderingForOrder
import cats.syntax.foldable._
import cats.syntax.semigroup._
import scala.collection.immutable.SortedMap
import scalatags.Text
import scalatags.Text.all._

import model.Countable
import model.Item
import prod.Factory
import prod.FactoryBlock

object FactoryView {
  def apply( solution: Either[String, Factory] ): Text.TypedTag[String] =
    fieldset(
      legend( "Factory" ),
      solution.fold[Frag](
        err => s"Failed to find a solution: $err",
        factory =>
          Seq(
            recipeTable( factory ),
            extractedResources( factory ),
            itemsInOutView( factory )
          )
      )
    )

  val headers =
    Vector(
      ( 1, "Nb." ),
      ( 2, "Recipe" ),
      ( 2, "Machines" ),
      ( 1, "Rate" ),
      ( 2, "Power" )
    )

  def recipeRow( block: FactoryBlock ): Text.TypedTag[String] = {
    import block._

    val recipeName  = recipe.item.displayName
    val altPrefix   = "Alternate: "
    val isAlternate = recipeName.startsWith( altPrefix )

    tr(
      td( f"$itemAmount%4.3f" ),
      td( recipe.item.displayName.stripPrefix( altPrefix ), colspan := (if (isAlternate) 1 else 2) ),
      Option.when( isAlternate )( td( "ALT" ) ),
      td( f"$machineCount%3d" ),
      td( machine.displayName ),
      td( f"$itemAmountPerUnit%3.3f / unit @ ${clockSpeedMillionth / 10000}%3d.${clockSpeedMillionth % 10000}%04d %%" ),
      td( f"$power%4.2f" ),
      td( "MW" )
    )
  }

  def recipeTable( factory: Factory ): Text.TypedTag[String] =
    table(
      colgroup(
        col( textAlign := "right" ),
        col( textAlign := "left" ),
        col( textAlign := "right" ),
        col( textAlign := "right" ),
        col( textAlign := "left" ),
        col( textAlign := "left" ),
        col( textAlign := "right" ),
        col( textAlign := "left" )
      ),
      thead(
        tr(
          headers.map { case ( w, h ) => th( colspan := w, h ) }
        )
      ),
      tbody(
        factory.blocks.map( recipeRow ),
        tr(
          td( colspan := 6, textAlign := "right", "Total Power" ),
          td( f"${factory.blocks.foldMap( _.power )}%4.2f" ),
          td( "MW" )
        )
      )
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

  def itemsInOut( factory: Factory ): SortedMap[Item, ( Map[String, Double], Map[String, Double] )] =
    factory.blocks.foldMap(
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
      factory.bill.items.foldMap {
        case Countable( item, amount ) =>
          SortedMap(
            (
              item,
              ( Map( ( "requested", amount ) ), Map.empty[String, Double] )
            )
          )
      } |+|
      factory.extraOutputs.foldMap {
        case Countable( item, amount ) =>
          SortedMap(
            (
              item,
              ( Map( ( "extra", amount ) ), Map.empty[String, Double] )
            )
          )
      }

  def itemsInOutView( factory: Factory ): Text.TypedTag[String] =
    div(
      itemsInOut( factory ).toVector.map { case ( item, ( to, from ) ) => itemInOut( item, to, from ) }
    )

  def extractedResources( factory: Factory ): Text.TypedTag[String] =
    table(
      factory.blocks
        .filter( _.recipe.item.isExtraction )
        .foldMap { block =>
          val product = block.recipe.item.productsPerMinute.head
          Map( ( product.item, block.recipe.amount * product.amount ) )
        }
        .toVector
        .sortBy { case ( p, x ) => ( -x, p ) }
        .map {
          case ( p, x ) =>
            tr(
              td( textAlign := "right", f"$x%.3f" ),
              td( p.displayName )
            )
        }
    )
}
