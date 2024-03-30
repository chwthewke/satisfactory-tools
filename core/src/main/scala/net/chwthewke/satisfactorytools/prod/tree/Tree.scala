package net.chwthewke.satisfactorytools
package prod
package tree

import cats.Eval
import cats.free.Cofree

object Tree {
  val empty: Tree = Cofree[Vector, Vector[ClockedRecipe]]( Vector.empty, Eval.now( Vector.empty ) )

  def addRoot( tree: Tree, recipes: Vector[ClockedRecipe] ): Tree =
    Cofree(
      ( tree.head ++ recipes ).map( _.recipe ).gather.map( ClockedRecipe.roundUp ),
      tree.tail
    )

  def addRoot( tree: Tree, recipe: ClockedRecipe ): Tree = addRoot( tree, Vector( recipe ) )
}
