package net.chwthewke.satisfactorytools
package model

import alleycats.std.map._
import cats.Show
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.show._
import cats.syntax.traverse._

import data.MapConfig

case class MapOptions( resourceNodes: Map[ExtractorType, Map[Item, ResourceDistrib]] ) {
  def get( machine: Machine, item: Item ): ResourceDistrib =
    machine.machineType.extractorType.flatMap( resourceNodes.get ).flatMap( _.get( item ) ).orEmpty
}

object MapOptions {
  def init( modelItems: Map[ClassName, Item], config: MapConfig ): Either[String, MapOptions] =
    config.resourceNodes
      .traverse(
        _.toVector
          .traverse {
            case ( itemClass, distrib ) =>
              modelItems.get( itemClass ).toValidNel( itemClass ).tupleRight( distrib )
          }
          .map( _.toMap )
      )
      .map( MapOptions( _ ) )
      .leftMap( _.mkString_( "Unknown items in resource nodes config: ", ", ", "" ) )
      .toEither

  implicit val mapOptionsShow: Show[MapOptions] = {
    def showItem( item: Item, distrib: ResourceDistrib ): String =
      show"${item.displayName.padTo( 20, ' ' )} P ${f"${distrib.pureNodes}% 2d"} " +
        show"N ${f"${distrib.normalNodes}% 2d"} I ${f"${distrib.impureNodes}% 2d"}"

    def showExtractorType( extractorType: ExtractorType, items: Map[Item, ResourceDistrib] ): String =
      items.toVector.map( (showItem _).tupled ).mkString_( show"${extractorType.description}\n  ", "\n  ", "" )

    Show.show(
      opts => opts.resourceNodes.toVector.map( (showExtractorType _).tupled ).mkString_( "\n\n" )
    )
  }
}
