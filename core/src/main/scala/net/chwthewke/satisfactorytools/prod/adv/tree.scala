package net.chwthewke.satisfactorytools
package prod
package adv

import cats.Eval
import cats.Monoid
import cats.PartialOrder
import cats.Show
import cats.data.NonEmptyVector
import cats.derived.semiauto
import cats.free.Cofree
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.functorFilter._
import cats.syntax.option._
import cats.syntax.partialOrder._
import cats.syntax.traverse._
import cats.syntax.vector._

import data.ClassName
import net.chwthewke.satisfactorytools.data.Countable
import net.chwthewke.satisfactorytools.prod.Calculator.Tolerance

object tree {

  //////////////////
  /* Factory tree
   *
   * Goal: specify a split of the recipe list in a tree structure
   *
   * Required actions:
   *   - push down a recipe, into an existing or a new child node
   *     - push down a fraction of a recipe, or a specific amount of products
   *     - "pull" down the required amounts of ingredients to a recipe in a child node
   *   - pull up a recipe
   *   - destroy a sub-tree
   *
   * Optional actions:
   *   - force the recipes in a node to be a multiple of some *n >= 1*
   *   - reordering things
   */

  sealed abstract class TreeLoc( val indices: Vector[Int] ) {
    def append( ix: Int ): TreeLoc.NonRoot =
      TreeLoc.NonRoot( indices.toNev.fold( NonEmptyVector.one( ix ) )( _.append( ix ) ) )
    def prepend( ix: Int ): TreeLoc.NonRoot =
      TreeLoc.NonRoot( NonEmptyVector( ix, indices ) )

    def nonRoot: Option[TreeLoc.NonRoot]

    override def toString: String = indices.map( _ + 1 ).mkString_( "." )
  }

  object TreeLoc {
    def apply( indices: Vector[Int] ): TreeLoc =
      indices.toNev.fold[TreeLoc]( Root )( NonRoot( _ ) )

    def parse( str: String ): Option[TreeLoc] =
      str
        .split( '.' )
        .toVector
        .filter( _.nonEmpty )
        .traverse( _.toIntOption.map( _ - 1 ).filter( _ >= 0 ) )
        .map( TreeLoc( _ ) )

    implicit val treeLocPartialOrder: PartialOrder[TreeLoc] =
      PartialOrder.from(
        ( l, m ) =>
          if (l == m) 0d
          else if (l.indices.startsWith( m.indices )) 1d
          else if (m.indices.startsWith( l.indices )) -1d
          else Double.NaN
      )

    implicit val treeLocShow: Show[TreeLoc] = Show.fromToString

    case object Root extends TreeLoc( Vector.empty ) {
      override def nonRoot: Option[NonRoot] = None
    }
    case class NonRoot( path: NonEmptyVector[Int] ) extends TreeLoc( path.toVector ) {
      def head: Int     = path.head
      def tail: TreeLoc = TreeLoc( path.tail )
      def init: TreeLoc = TreeLoc( path.init )
      def last: Int     = path.last

      override def nonRoot: Option[NonRoot] = Some( this )
    }

    object NonRoot {
      def unapply( loc: NonRoot ): Some[( Int, TreeLoc )] =
        Some( ( loc.head, loc.tail ) )
    }
  }

  sealed trait TreeCommand

  object TreeCommand {
    case class PushDown( to: TreeLoc.NonRoot, recipe: ClassName, `type`: PushDownType ) extends TreeCommand
    case class PullUp( from: TreeLoc.NonRoot, recipe: ClassName )                       extends TreeCommand
    case class Destroy( at: TreeLoc.NonRoot )                                           extends TreeCommand

    implicit val treeCommandShow: Show[TreeCommand] = semiauto.show[TreeCommand]
  }

  case class TreeCommands( commands: Vector[TreeCommand] ) extends AnyVal

  object TreeCommands {
    val empty: TreeCommands = TreeCommands( Vector.empty )

    def apply( command: TreeCommand ): TreeCommands = TreeCommands( Vector( command ) )

    implicit val treeCommandsMonoid: Monoid[TreeCommands] =
      new Monoid[TreeCommands] {
        override def empty: TreeCommands = TreeCommands.empty

        override def combine( x: TreeCommands, y: TreeCommands ): TreeCommands =
          TreeCommands( y.commands.foldLeft( x.commands )( appendCommand ) )
      }

    implicit val treeCommandsShow: Show[TreeCommands] =
      Show.show( _.commands.mkString_( " | " ) )
  }

  def appendCommand[C >: TreeCommand.PushDown <: TreeCommand]( commands: Vector[C], command: TreeCommand ): Vector[C] =
    command match {
      case TreeCommand.PullUp( from, recipe ) =>
        // TODO this recursion could be a filter
        ( commands, Vector.empty[C] )
          .tailRecM[Eval, Vector[C]] {
            case ( rest, acc ) =>
              if (rest.isEmpty)
                Eval.now( Right( acc ) )
              else {
                rest.last match {
                  case TreeCommand.PushDown( to, recipe1, _ ) if (from: TreeLoc) == to && recipe == recipe1 =>
                    Eval.later( Left( ( rest.init, acc ) ) )
                  case _ =>
                    Eval.later( Left( ( rest.init, rest.last +: acc ) ) )
                }
              }
          }
          .value

      case TreeCommand.Destroy( at ) =>
        ( commands, Vector.empty[C] )
          .tailRecM[Eval, Vector[C]] {
            case ( rest, acc ) =>
              if (rest.isEmpty)
                Eval.now( Right( acc ) )
              else {
                rest.last match {
                  case TreeCommand.PushDown( to, _, _ ) if (at: TreeLoc) <= to =>
                    Eval.later( Left( ( rest.init, acc ) ) )
                  case _ =>
                    Eval.later( Left( ( rest.init, rest.last +: acc ) ) )
                }
              }
          }
          .value
      case c @ TreeCommand.PushDown( _, _, _ ) => commands :+ c
    }

  sealed trait PushDownType

  object PushDownType {
    case object Full                    extends PushDownType // same as Fraction(1) for practicality
    case class Fraction( n: Int )       extends PushDownType
    case class Amount( amount: Double ) extends PushDownType
    case class For( recipe: ClassName ) extends PushDownType

    implicit val pushDownTypeShow: Show[PushDownType] = semiauto.show[PushDownType]
  }

  type Tree = Cofree[Vector, Vector[ClockedRecipe]]

  private val empty: Tree = Cofree[Vector, Vector[ClockedRecipe]]( Vector.empty, Eval.now( Vector.empty ) )

  private def addRoot( tree: Tree, recipes: Vector[ClockedRecipe] ): Tree =
    Cofree(
      (tree.head ++ recipes).map( _.recipe ).gather.map( ClockedRecipe.roundUp ),
      tree.tail
    )

  private def addRoot( tree: Tree, recipe: ClockedRecipe ): Tree =
    addRoot( tree, Vector( recipe ) )

  case class FactoryTree( tree: Tree ) {

    def run( command: TreeCommand ): Option[FactoryTree] = command match {
      case TreeCommand.PushDown( to, recipe, pdt ) =>
        modify( to.init )(
          sub => pushDown( sub, sub.head.indexWhere( _.recipe.item.className == recipe ), to.last, pdt )
        )

      case TreeCommand.PullUp( from, recipe ) =>
        modify( from.init ) { sub =>
          val children: Vector[Tree] = sub.tailForced
          children
            .lift( from.last )
            .flatMap { child =>
              val ix = child.head.indexWhere( _.recipe.item.className == recipe )
              child.head.lift( ix ).map { pulled =>
                val newChild: Tree = Cofree( child.head.patch( ix, Nil, 1 ), child.tail )
                addRoot(
                  Cofree( sub.head, Eval.now( children.updated( from.last, newChild ) ) ),
                  pulled
                )
              }
            }
        }

      case TreeCommand.Destroy( at ) =>
        modify( at.init ) { sub =>
          val children: Vector[Tree] = sub.tailForced

          children.lift( at.last ).map { child =>
            val pulled: Vector[ClockedRecipe] = Cofree
              .cata[Vector, Vector[ClockedRecipe], Vector[ClockedRecipe]]( child )(
                ( recipes, children ) => Eval.now( recipes ++ children.flatten )
              )
              .value

            addRoot(
              Cofree(
                sub.head,
                Eval.now( children.patch( at.last, Nil, 1 ) )
              ),
              pulled
            )
          }
        }
    }

    private def pushDown( sub: Tree, ix: Int, to: Int, `type`: PushDownType ): Option[Tree] =
      sub.head.lift( ix ).flatMap { recipe =>
        val children: Vector[Tree] = sub.tailForced
        if (to < 0 || to > children.size)
          none
        else {
          val target: Option[Tree] = children.lift( to )

          pushedAndRemaining( recipe, `type`, target ).map {
            case ( pushed, remaining ) =>
              Cofree(
                sub.head.patch( ix, remaining, 1 ),
                Eval.now[Vector[Tree]](
                  target
                    .fold( children :+ addRoot( empty, pushed ) )( t => children.updated( to, addRoot( t, pushed ) ) )
                )
              )
          }
        }
      }

    /**
      * tries to select what part of the source `recipe` to push down, according to the `PushDownType` `type` and
      *     the target node `target` (used when `type` is `PushDownType.For( consumer )`)
      */
    private def pushedAndRemaining(
        recipe: ClockedRecipe,
        `type`: PushDownType,
        target: Option[Tree]
    ): Option[( ClockedRecipe, Option[ClockedRecipe] )] = `type` match {
      case PushDownType.Full =>
        ( recipe, none ).some

      case PushDownType.Fraction( n ) =>
        split( recipe, recipe.fractionalAmount / n.toDouble ).some

      case PushDownType.Amount( amount ) =>
        split( recipe, amount / recipe.mainProductAmount ).some

      case PushDownType.For( consumer ) =>
        target.flatMap { sub =>
          val ix: Int = sub.head.indexWhere( _.recipe.item.className == consumer )

          sub.head.lift( ix ).flatMap { targetRecipe =>
            val pushedCount: Double =
              targetRecipe.ingredientsPerMinute
                .mapFilter { ingredient =>
                  recipe.recipe
                    .map( _.productsPerMinute.find( _.item == ingredient.item ) )
                    .sequence
                    .map {
                      case Countable( Countable( _, perUnit ), unitCount ) =>
                        (ingredient.amount / perUnit).min( unitCount )
                    }
                }
                .maximumOption
                .orEmpty

            Option.when( pushedCount > 0 )( split( recipe, pushedCount ) )
          }
        }
    }

    private def split( recipe: ClockedRecipe, count: Double ): ( ClockedRecipe, Option[ClockedRecipe] ) =
      if (count >= recipe.fractionalAmount - Tolerance)
        ( recipe, none )
      else
        (
          ClockedRecipe.roundUp( recipe.recipe.withAmount( count ) ),
          ClockedRecipe.roundUp( recipe.recipe.mapAmount( _ - count ) ).some
        )

    private def modify( treeLoc: TreeLoc )( f: Tree => Option[Tree] ): Option[FactoryTree] =
      ( treeLoc, tree, identity[Tree]( _ ) )
        .tailRecM[Eval, Option[Tree]] {
          case ( l, t, k ) =>
            l match {
              case TreeLoc.Root => Eval.now( Right( f( t ).map( k ) ) )
              case TreeLoc.NonRoot( head, tail ) =>
                t.tail.map { children =>
                  if (head >= 0 && head < children.size)
                    Left(
                      (
                        tail,
                        children( head ),
                        k.compose( q => Cofree( t.head, Eval.now( children.updated( head, q ) ) ) )
                      )
                    )
                  else
                    Right( none[Tree] )
                }
            }
        }
        .value
        .map( FactoryTree( _ ) )

  }

  object FactoryTree {
    def apply( recipes: Vector[ClockedRecipe] ): FactoryTree =
      FactoryTree( addRoot( empty, recipes ) )
  }
}
