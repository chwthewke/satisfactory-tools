package net.chwthewke.satisfactorytools
package prod

import cats.syntax.foldable._

import model.Item
import model.Machine
import model.Model
import model.Options
import model.Recipe

case class ResourceCaps( caps: Map[Item, Double] )

object ResourceCaps {
  def init( model: Model, options: Options, extractionRecipes: Map[Item, Recipe[Machine, Item]] ): ResourceCaps =
    ResourceCaps(
      model.resourceNodes.toVector.foldMap {
        case ( extractor, byItem ) =>
          byItem
            .flatMap {
              case ( item, dist ) =>
                extractionRecipes
                  .get( item )
                  .filter( _.producers.exists( _.className == extractor.className ) )
                  .map( recipe => ( item, dist.value( recipe, options.clockSpeed, options.belt ) ) )
            }
      }
    )
}
