package net.chwthewke.satisfactorytools
package prod

import model.Item

case class ResourceWeights( weights: Map[Item, Double] )

object ResourceWeights {
  def init( caps: ResourceCaps ): ResourceWeights =
    ResourceWeights( caps.caps.map { case ( item, cap ) => ( item, 1e6 / cap ) } )
}
