package net.chwthewke.satisfactorytools
package prod

import cats.Show
import com.flowtick.graphs._
import com.flowtick.graphs.defaults._
//
import model.Item
import model.Machine
import model.Recipe

case class RecipeGraph( graph: Graph[Unit, RecipeGraph.N] )

object RecipeGraph {
  sealed trait N extends Product with Serializable

  final case class ItemNode( item: Item )                      extends N
  final case class RecipeNode( recipe: Recipe[Machine, Item] ) extends N

  def itemNode( item: Item ): N                      = ItemNode( item )
  def recipeNode( recipe: Recipe[Machine, Item] ): N = RecipeNode( recipe )

  object N {
    implicit val showN: Show[N] = Show.show {
      case ItemNode( item )     => s"I[${item.displayName}]"
      case RecipeNode( recipe ) => s"R[${recipe.displayName}]"
    }
  }

  def of( recipes: Vector[Recipe[Machine, Item]] ): RecipeGraph =
    RecipeGraph(
      Graph.fromEdges(
        recipes
          .sortBy( r => ( r.products.head.item.className.name, r.className.name ) )
          .flatMap(
            rec =>
              rec.ingredients.map( ci => recipeNode( rec ) --> itemNode( ci.item ) ) ++
                rec.products.toList.map( ci => itemNode( ci.item ) --> recipeNode( rec ) )
          )
      )
    )

}
