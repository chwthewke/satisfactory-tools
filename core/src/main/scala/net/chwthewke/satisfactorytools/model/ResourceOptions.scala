package net.chwthewke.satisfactorytools
package model

import cats.Eq
import cats.Show
import cats.syntax.foldable._
import cats.syntax.option._
import cats.syntax.show._

import data.Item

case class ResourceOptions(
    resourceNodes: Map[ExtractorType, Map[Item, ResourceDistrib]],
    resourceWeights: ResourceWeights
) {
  def get( machine: Machine, item: Item ): ResourceDistrib =
    machine.machineType.extractorType.flatMap( resourceNodes.get ).flatMap( _.get( item ) ).orEmpty
}

object ResourceOptions {

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

  implicit val resourceOptionsEq: Eq[ResourceOptions] = Eq.by(
    ro =>
      (
        ro.resourceNodes.map { case ( t, m ) => ( t, m.map { case ( i, d ) => ( i.className, d ) } ) },
        ro.resourceWeights
      )
  )
}
