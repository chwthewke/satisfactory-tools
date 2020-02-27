package net.chwthewke.satisfactorytools
package prod

import cats.Show
import com.flowtick.graphs._
import com.flowtick.graphs.defaults._
//
import model.Item
import model.Machine
import model.Recipe

object RecipeGraph {
  sealed trait N extends Product with Serializable

  final case class ItemNode( item: Item )                      extends N
  final case class RecipeNode( recipe: Recipe[Machine, Item] ) extends N

  object N {
    implicit val showN: Show[N] = Show.show {
      case ItemNode( item )     => s"I[${item.displayName}]"
      case RecipeNode( recipe ) => s"R[${recipe.displayName}]"
    }
  }

  def itemNode( item: Item ): N                      = ItemNode( item )
  def recipeNode( recipe: Recipe[Machine, Item] ): N = RecipeNode( recipe )

  def of( recipes: Vector[Recipe[Machine, Item]] ): Graph[Unit, N, Unit] = Graph.fromEdges(
    recipes
      .sortBy( r => ( r.product.head.item.className.name, r.className.name ) )
      .flatMap(
        rec =>
          rec.ingredients.map( ci => n( recipeNode( rec ) ) --> n( itemNode( ci.item ) ) ) ++
            rec.product.toList.map( ci => n( itemNode( ci.item ) ) --> n( recipeNode( rec ) ) )
      )
  )
}
