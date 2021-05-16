package net.chwthewke.satisfactorytools
package model

import alleycats.std.map._
import cats.Show
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.show._
import cats.syntax.traverse._

import data.ClassName
import data.Item

case class ResourceOptions(
    resourceNodes: Map[ExtractorType, Map[Item, ResourceDistrib]],
    resourceWeights: ResourceWeights
) {
  def get( machine: Machine, item: Item ): ResourceDistrib =
    machine.machineType.extractorType.flatMap( resourceNodes.get ).flatMap( _.get( item ) ).orEmpty
}

object ResourceOptions {
  def init( modelItems: Map[ClassName, Item], config: MapConfig ): Either[String, ResourceOptions] =
    config.resourceNodes
      .traverse(
        _.toVector
          .traverse {
            case ( itemClass, distrib ) =>
              modelItems.get( itemClass ).toValidNel( itemClass ).tupleRight( distrib )
          }
          .map( _.toMap )
      )
      .map( ResourceOptions( _, ResourceWeights.default ) )
      .leftMap( _.mkString_( "Unknown items in resource nodes config: ", ", ", "" ) )
      .toEither

  implicit val resourceOptionsShow: Show[ResourceOptions] = {
    def showItem( item: Item, distrib: ResourceDistrib ): String =
      show"${item.displayName.padTo( 20, ' ' )} P ${f"${distrib.pureNodes}% 2d"} " +
        show"N ${f"${distrib.normalNodes}% 2d"} I ${f"${distrib.impureNodes}% 2d"}"

    def showExtractorType( extractorType: ExtractorType, items: Map[Item, ResourceDistrib] ): String =
      items.toVector.map( (showItem _).tupled ).mkString_( show"${extractorType.description}\n  ", "\n  ", "" )

    Show.show(
      opts =>
        show"""NODES
              |${opts.resourceNodes.toVector.map( (showExtractorType _).tupled ).mkString_( "\n\n" )}
              |
              |WEIGHTS
              |${opts.resourceWeights.weights
                .map { case ( item, weight ) => show"${item.displayName}: $weight" }
                .mkString( "\n" )}
              |""".stripMargin
    )
  }
}
