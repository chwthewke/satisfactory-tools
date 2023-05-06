package net.chwthewke.satisfactorytools
package prod
package adv

import cats.Semigroup
import cats.syntax.option._
import cats.syntax.order._
import cats.syntax.semigroup._
import scala.annotation.tailrec

import net.chwthewke.satisfactorytools.data.ClassName
import net.chwthewke.satisfactorytools.data.Countable
import net.chwthewke.satisfactorytools.data.Item
import net.chwthewke.satisfactorytools.model.Options.ClockSpeed
import net.chwthewke.satisfactorytools.model.Recipe

sealed abstract class SplitRecipe

object SplitRecipe {
  case class Success( result: RichRecipe, remainder: Option[RichRecipe] ) extends SplitRecipe
  case class Partial( result: RichRecipe, fulfilled: Double )             extends SplitRecipe
  case object Failure                                                     extends SplitRecipe
}

case class RichRecipe( block: ClockedRecipe, split: Int, maxClockSpeed: ClockSpeed ) {
  def recipe: Recipe           = block.recipe.item
  def className: ClassName     = recipe.className
  def fractionalAmount: Double = block.fractionalAmount

  def resplit( parts: Int ): RichRecipe = {
    val baseAmount: Int =
      (block.fractionalAmount * 100d / maxClockSpeed.percent).ceil.toInt

    val splitAmount: Int = {
      val r: Int = baseAmount % parts
      if (r == 0)
        baseAmount
      else
        baseAmount + parts - r
    }

    RichRecipe(
      ClockedRecipe.fixed( block.recipe.item, block.fractionalAmount, splitAmount ),
      parts,
      maxClockSpeed
    )
  }

  private def part( amount: Double ): RichRecipe =
    RichRecipe(
      ClockedRecipe.roundUp( Countable( block.recipe.item, amount ) ),
      1,
      maxClockSpeed
    )

  def splitProductPerMinute( item: ClassName, goalAmount: Double ): SplitRecipe = {
    recipe.productsPerMinute
      .collectFirst {
        case Countable( it, amt ) if it.className == item =>
          val producedAmount = block.fractionalAmount * amt
          if (producedAmount == goalAmount)
            SplitRecipe.Success( this, None )
          else if (producedAmount < goalAmount)
            SplitRecipe.Partial( this, producedAmount )
          else {
            val remainingCount: Double = goalAmount / amt - block.fractionalAmount
            SplitRecipe.Success( this, Some( part( remainingCount ) ) )
          }
      }
      .getOrElse( SplitRecipe.Failure )

  }
}

object RichRecipe {
  implicit val richRecipeSemigroup: Semigroup[RichRecipe] =
    ( r, s ) =>
      RichRecipe(
        ClockedRecipe.roundUp( Countable( r.recipe, r.fractionalAmount + s.fractionalAmount ) ),
        1,
        r.maxClockSpeed.min( s.maxClockSpeed )
      ).resplit( 1 )

}

case class FactoryTree(
    blocks: Vector[RichRecipe],
    subTrees: Vector[FactoryTree]
) {
  def add( recipe: RichRecipe ): FactoryTree = {
    copy(
      blocks = blocks
        .updateWhere( _.className == recipe.className )( _ |+| recipe )
        .getOrElse( blocks :+ recipe )
    )
  }

}

object FactoryTree {
  val empty: FactoryTree = FactoryTree( Vector.empty, Vector.empty )
}

sealed abstract class Command {
  def run( tree: FactoryTree ): Option[FactoryTree] = None
}

object Command {
  trait Push extends Command

  case class Split( recipe: ClassName, parts: Int ) extends Command {
    override def run( tree: FactoryTree ): Option[FactoryTree] =
      tree.blocks
        .updateWhere( _.className == recipe )( _.resplit( parts ) )
        .map( block => tree.copy( blocks = block ) )
  }

  case class MoveSingle( recipe: ClassName, to: Int ) extends Command.Push {
    override def run( tree: FactoryTree ): Option[FactoryTree] = {
      val ( toMove, newBlock ) =
        tree.blocks.partition( _.className == recipe )

      toMove match {
        case Vector( single ) =>
          FactoryTree(
            newBlock,
            tree.subTrees.patch(
              to,
              Vector( tree.subTrees.lift( to ).getOrElse( FactoryTree.empty ).add( single ) ),
              1
            )
          ).some
        case _ => none
      }
    }
  }

  case class MoveAll( recipe: ClassName, to: Int ) extends Command.Push {
    private def partitionTarget(
        block: Vector[RichRecipe],
        input: Countable[Double, Item]
    ): ( Vector[RichRecipe], Vector[RichRecipe] ) = {
      @tailrec
      def loop(
          goal: Countable[Double, Item],
          sources: List[RichRecipe],
          found: Vector[RichRecipe],
          exhausted: Vector[RichRecipe]
      ): ( Vector[RichRecipe], Vector[RichRecipe] ) = {
        if (goal.amount <= 0d)
          ( found, exhausted ++ sources.toVector )
        else {
          sources match {
            case Nil => ( found, exhausted )
            case source :: moreSources =>
              source.splitProductPerMinute( goal.item.className, goal.amount ) match {
                case SplitRecipe.Success( f, rem ) =>
                  loop( goal.mapAmount( _ => 0d ), moreSources, found :+ f, exhausted ++ rem )
                case SplitRecipe.Partial( f, d ) =>
                  loop( goal.mapAmount( _ - d ), moreSources, found :+ f, exhausted )
                case SplitRecipe.Failure =>
                  loop( goal, moreSources, found, exhausted :+ source )
              }
          }
        }
      }

      loop( input, block.toList, Vector.empty, Vector.empty )
    }

    override def run( tree: FactoryTree ): Option[FactoryTree] = {
      val ( toMoveFirst, block1 ) =
        tree.blocks.partition( _.className == recipe )

      toMoveFirst match {
        case Vector( single ) =>
          @tailrec
          def loop(
              moved: Vector[RichRecipe],
              remaining: Vector[RichRecipe],
              inputs: List[Countable[Double, Item]]
          ): ( Vector[RichRecipe], Vector[RichRecipe] ) =
            inputs match {
              case Nil =>
                ( moved, remaining )
              case input :: moreInputs =>
                val ( toMove, rest ) = partitionTarget( remaining, input )
                loop(
                  moved ++ toMove,
                  rest,
                  (moreInputs ++ toMove.flatMap( _.block.ingredientsPerMinute )).gather
                )
            }

          val ( toMoveAll, newBlock ) =
            loop( Vector( single ), block1, single.block.ingredientsPerMinute )

          FactoryTree(
            newBlock,
            tree.subTrees.patch(
              to,
              Vector( toMoveAll.foldLeft( tree.subTrees.lift( to ).getOrElse( FactoryTree.empty ) )( _.add( _ ) ) ),
              1
            )
          ).some

        case _ => none
      }

    }
  }

}
