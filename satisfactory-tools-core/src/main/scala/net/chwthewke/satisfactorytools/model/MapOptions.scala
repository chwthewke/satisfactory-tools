package net.chwthewke.satisfactorytools
package model

import alleycats.std.map._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.traverse._

import data.MapConfig

case class MapOptions( resourceNodes: Map[ExtractorType, Map[Item, ResourceDistrib]] ) {
  def get( machine: Machine, item: Item ): ResourceDistrib =
    machine.machineType.extractorType.flatMap( resourceNodes.get ).flatMap( _.get( item ) ).orEmpty
}

object MapOptions {
  def init( model: Model, config: MapConfig ): Either[String, MapOptions] =
    config.resourceNodes
      .traverse(
        _.toVector
          .traverse {
            case ( itemClass, distrib ) =>
              model.items.get( itemClass ).toValidNel( itemClass ).tupleRight( distrib )
          }
          .map( _.toMap )
      )
      .map( MapOptions( _ ) )
      .leftMap( _.mkString_( "Unknown items in resource nodes config: ", ", ", "" ) )
      .toEither
}
