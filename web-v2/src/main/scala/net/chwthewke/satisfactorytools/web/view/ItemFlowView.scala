package net.chwthewke.satisfactorytools
package web.view

import cats.Comparison
import cats.syntax.foldable._
import cats.syntax.functorFilter._
import scala.annotation.tailrec
import scala.collection.immutable.SortedSet
import scalatags.Text.Tag

import data.Countable
import data.Item
import net.chwthewke.satisfactorytools.model.Recipe
import net.chwthewke.satisfactorytools.protocol.OutputTab.ItemFlow.ItemState
import net.chwthewke.satisfactorytools.protocol.RecipeId
import prod.ClockedRecipe
import protocol.OutputTab.ItemFlow
import web.forms.Actions

case class ItemFlowView( state: ItemFlow.State ) extends ( ( ItemFlow.Data, Int ) => Tag ) {
  import scalatags.Text.all._

  import ItemFlowView._

  private def changeStateLink( newState: ItemFlow.State ): Modifier =
    formaction := s"output/${Actions.output( ItemFlow( newState ) )}"

  private def setCurrentItem( data: ItemFlow.Data, item: Item ): Modifier =
    changeStateLink( state.copy( currentItem = data.itemIdsByClass.get( item.className ) ) )

  private def showItemFlow( data: ItemFlow.Data, item: Item, state: ItemState ): Frag = {
    def indexOf( recipe: Recipe, order: Vector[RecipeId] ): Int =
      data.recipeIdsByClass.get( recipe.className ).fold( -1 )( order.indexOf )

    val producers: Vector[Producer] = data.factory.allRecipes
      .mapFilter( r =>
        r.productsPerMinute.find( _.item.className == item.className ).map( x => Producer( r, x.amount ) )
      )
      .sortBy( p => indexOf( p.recipe.recipe.item, state.producerRecipeOrder ) )

    val consumers: Vector[Consumer] = data.factory.allRecipes
      .mapFilter( r =>
        r.ingredientsPerMinute.find( _.item.className == item.className ).map( x => Consumer.Recipe( r, x.amount ) )
      ) ++
      data.factory.extraOutputs.find( _.item.className == item.className ).map( Consumer.ByProduct ) ++
      data.bill.items.find( _.item.className == item.className ).map( Consumer.Requested )

    div(
      h4( s"Item flow for ${item.displayName}" ),
      stacksTable( RecipeStacks( producers, consumers ) )
    )
  }

  def stacksCell[R <: ItemFlowView.Block]( slot: RecipeSlot[R], rowIndex: RowIndex, rightSide: Boolean ): Modifier = {

    def cellHeight( pxAdjust: Int ): String =
      s"calc( ${slot.height * 1.5f}em + ${( slot.height - 1 ) * 2 + pxAdjust}px )"

    def sideTag: String = if (rightSide) "R" else "L"

    Table.groupedSortableCell(
      startsGroup = false,
      rowIndex,
      s"flow_up_${sideTag}_${rowIndex.index}",
      s"flow_down_${sideTag}_${rowIndex.index}",
      s"flow_group_${sideTag}_${rowIndex.index}",
      buttonsOnRight = rightSide
    )(
      padding := "0",
      rowspan := slot.height.toString,
      height  := cellHeight( 0 ),
      div(
        height  := cellHeight( -2 ),
        padding := "1px",
        display := "grid",
        div(
          alignSelf.center,
          slot.recipe.repr
        )
      )
    )
  }

  def stacksTable( stacks: RecipeStacks ): Tag =
    table(
      `class` := "has-borders full-width stacks",
      thead(
        tr(
          th( "Producers" ),
          th( "Consumers" )
        )
      ),
      tbody(
        0.until( stacks.height )
          .map { ix =>
            val rowIndex: RowIndex = RowIndex( ix, stacks.height )
            tr(
              stacks.producers.byHeight.get( ix ).map( stacksCell( _, rowIndex, rightSide = false ) ),
              stacks.consumers.byHeight.get( ix ).map( stacksCell( _, rowIndex, rightSide = true ) )
            )
          }
      )
    )

  override def apply( data: ItemFlow.Data, groupCount: Int ): Tag = {
    val factoryItems: SortedSet[Item] =
      data.factory.allRecipes.foldMap( cr =>
        cr.recipe
          .foldMap( r => ( r.ingredients ++: r.products ).foldMap( c => SortedSet( c.item ) ) )
      )

    fieldset(
      legend( "Item flows" ),
      factoryItems.iterator
        .map( item =>
          button(
            `class` := "button is-small",
            setCurrentItem( data, item ),
            item.displayName
          )
        )
        .toSeq,
      state.currentItem.flatMap( data.itemsById.get ).map( showItemFlow( data, _ ) )
    )

  }
}

object ItemFlowView {
  sealed trait Block {
    def repr: String
    def amount: Double
  }

  trait RecipeRepr extends Block {
    def recipe: ClockedRecipe

    override def repr: String =
      f"${recipe.machineCount} ${recipe.recipe.item.displayName} @ ${recipe.clockSpeed}%.4f ($amount%.3f)"
  }

  case class Producer( recipe: ClockedRecipe, amount: Double ) extends Block with RecipeRepr

  sealed abstract class Consumer extends Block with Product with Serializable {
    def amount: Double
  }

  object Consumer {
    final case class Recipe( recipe: ClockedRecipe, amount: Double ) extends Consumer with RecipeRepr
    final case class ByProduct( contents: Countable[Double, Item] ) extends Consumer {
      override def repr: String   = f"By-product ($amount%.3f)"
      override def amount: Double = contents.amount
    }
    final case class Requested( contents: Countable[Double, Item] ) extends Consumer {
      override def repr: String   = f"Requested ($amount%.3f)"
      override def amount: Double = contents.amount
    }
  }

  case class RecipeSlot[R]( height: Int, recipe: R )
  case class RecipeStack[R <: Block]( slots: Vector[RecipeSlot[R]] ) {
    def isEmpty: Boolean            = slots.isEmpty
    def last: Option[RecipeSlot[R]] = slots.lastOption
    def height: Int                 = slots.foldMap( _.height )
    val amount: Double              = slots.foldMap( _.recipe.amount )

    def adjust( lastHeight: Int ): RecipeStack[R] =
      slots.lastOption.fold( this )( last => copy( slots = slots.init :+ last.copy( height = lastHeight ) ) )

    def push( recipe: R ): RecipeStack[R] = copy( slots = slots :+ RecipeSlot( 1, recipe ) )

    lazy val byHeight: Map[Int, RecipeSlot[R]] =
      slots
        .foldLeft( ( 0, Map.empty[Int, RecipeSlot[R]] ) ) {
          case ( ( h, acc ), s ) =>
            ( h + s.height, acc.updated( h, s ) )
        }
        ._2
  }

  object RecipeStack {
    def empty[R <: Block]: RecipeStack[R] = RecipeStack( Vector.empty )
  }

  private def compareApprox( lhs: Double, rhs: Double ): Comparison =
    if (math.abs( lhs - rhs ) < AmountTolerance) Comparison.EqualTo
    else if (lhs < rhs) Comparison.LessThan
    else Comparison.GreaterThan

  case class RecipeStacks( producers: RecipeStack[Producer], consumers: RecipeStack[Consumer] ) {

    def height: Int = producers.height.max( consumers.height )

    private def heightAdjustment( amountCmp: Comparison, heightCmp: Comparison ): Option[Int] = {
      val res = ( amountCmp, heightCmp ) match {
        case ( Comparison.LessThan, _ )                         => None
        case ( Comparison.EqualTo, Comparison.LessThan )        => Some( 0 )
        case ( Comparison.EqualTo, _ )                          => None
        case ( Comparison.GreaterThan, Comparison.GreaterThan ) => None
        case ( Comparison.GreaterThan, _ )                      => Some( 1 )
      }

      println( s"A-cmp $amountCmp H-cmp $heightCmp => $res" )
      res
    }

    private def logHA(): Unit =
      println( s"P[H=${producers.height} A=${producers.amount}] C[H=${consumers.height} A=${consumers.amount}]" )

    private def adjustProducers: RecipeStacks = {
      logHA()

      producers.last.fold( this ) { last =>
        val ph: Int = producers.height
        val ch: Int = consumers.height
        val lh: Int = last.height

        heightAdjustment(
          compareApprox( producers.amount, consumers.amount ),
          Comparison.fromInt( ph.compare( ch ) )
        )
          .map( n => ch - ph + lh + n ) // ph - lh + adjustment = ch + n
          .fold( this )( h => copy( producers = producers.adjust( h ) ) )
      }
    }

    private def adjustConsumers: RecipeStacks = {
      logHA()

      consumers.last.fold( this ) { last =>
        val ph: Int = producers.height
        val ch: Int = consumers.height
        val lh: Int = last.height

        heightAdjustment(
          compareApprox( consumers.amount, producers.amount ),
          Comparison.fromInt( ch.compare( ph ) )
        )
          .map( n => ph - ch + lh + n ) // ch - lh + adjustment = ph + n
          .fold( this )( h => copy( consumers = consumers.adjust( h ) ) )

      }
    }

    // TODO Can factor adjustProducers & adjustConsumers if we only call them through this
    // NOTE only one of adjustProducers, adjustConsumers can change this
    def adjustHeights: RecipeStacks = {
      val res = adjustProducers.adjustConsumers
      res.logHA()
      res
    }

    private def pushProducerUnsafe( producer: Producer ): RecipeStacks =
      copy( producers = producers.push( producer ) )

    def pushProducer( producer: Producer ): RecipeStacks = {
      println( s"push producer ${producer.repr}" )
      adjustHeights.pushProducerUnsafe( producer )
    }

    private def pushConsumerUnsafe( consumer: Consumer ): RecipeStacks =
      copy( consumers = consumers.push( consumer ) )

    def pushConsumer( consumer: Consumer ): RecipeStacks = {
      println( s"push consumer ${consumer.repr}" )
      adjustHeights.pushConsumerUnsafe( consumer )
    }
  }

  object RecipeStacks {
    val empty: RecipeStacks = RecipeStacks( RecipeStack.empty[Producer], RecipeStack.empty[Consumer] )

    @tailrec
    private def mkStacksRec(
        acc: RecipeStacks,
        producers: Vector[Producer],
        consumers: Vector[Consumer]
    ): RecipeStacks =
      (
        acc.producers.amount > acc.consumers.amount + AmountTolerance,
        consumers.headOption,
        producers.headOption
      ) match {
        case ( _, None, None ) => acc.adjustHeights
        case ( false, _, Some( p ) ) =>
          mkStacksRec( acc.pushProducer( p ), producers.tail, consumers )
        case ( true, Some( c ), _ ) =>
          mkStacksRec( acc.pushConsumer( c ), producers, consumers.tail )
        // the should-not-happen cases
        case ( _, _, Some( p ) ) =>
          mkStacksRec( acc.pushProducer( p ), producers.tail, consumers )
        case ( _, Some( c ), _ ) =>
          mkStacksRec( acc.pushConsumer( c ), producers, consumers.tail )
      }

    def apply( producers: Vector[Producer], consumers: Vector[Consumer] ): RecipeStacks =
      mkStacksRec( RecipeStacks.empty, producers, consumers )
  }
}
