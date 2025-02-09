package net.chwthewke.satisfactorytools
package prod
package tree

import cats.Eval
import cats.free.Cofree
import cats.syntax.all._

import data.Countable
import prod.Calculator.Tolerance

case class FactoryTree( tree: Tree ) {

  def runAll( commands: TreeCommands ): FactoryTree =
    commands.commands.foldLeft( this )( ( t, c ) => t.run( c ).getOrElse( t ) )

  def run( command: TreeCommand ): Option[FactoryTree] = command match {
    case TreeCommand.PushDown( to, recipe, pdt ) =>
      modify( to.init )( sub =>
        pushDown( sub, sub.head.indexWhere( _.recipe.item.className == recipe ), to.last, pdt )
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
              Tree.addRoot(
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
            .cata[Vector, Vector[ClockedRecipe], Vector[ClockedRecipe]]( child )( ( recipes, children ) =>
              Eval.now( recipes ++ children.flatten )
            )
            .value

          Tree.addRoot(
            Cofree(
              sub.head,
              Eval.now( children.patch( at.last, Nil, 1 ) )
            ),
            pulled
          )
        }
      }
  }

  private def pushDown( sub: Tree, ix: Int, to: Int, `type`: PushDownType ): Option[Tree] = {

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
                  .fold( children :+ Tree.addRoot( Tree.empty, pushed ) )( t =>
                    children.updated( to, Tree.addRoot( t, pushed ) )
                  )
              )
            )
        }
      }
    }
  }

  /**
   * tries to select what part of the source `recipe` to push down, according to the `PushDownType` `type` and the
   * target node `target` (used when `type` is `PushDownType.For( consumer )`)
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
      if (amount > Tolerance)
        split( recipe, amount * recipe.fractionalAmount / recipe.mainProductAmount ).some
      else
        none

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
                      ( ingredient.amount / perUnit ).min( unitCount )
                  }
              }
              .maximumOption
              .orEmpty

          Option.when( pushedCount > Tolerance )( split( recipe, pushedCount ) )
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
    FactoryTree( Tree.addRoot( Tree.empty, recipes ) )
}
