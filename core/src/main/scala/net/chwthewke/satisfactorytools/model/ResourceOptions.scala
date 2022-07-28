package net.chwthewke.satisfactorytools
package model

import cats.Eq
import cats.Show
import cats.syntax.foldable._
import cats.syntax.option._
import cats.syntax.show._
import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

import data.ClassName
import data.Item

case class ResourceOptions(
    resourceNodes: Map[ExtractorType, Map[ClassName, ResourceDistrib]],
    resourceWeights: ResourceWeights
) {
  def get( machine: Machine, item: Item ): ResourceDistrib =
    machine.machineType.extractorType.flatMap( resourceNodes.get ).flatMap( _.get( item.className ) ).orEmpty
}

object ResourceOptions {

  implicit val resourceOptionsShow: Show[ResourceOptions] = {
    def showItem( item: ClassName, distrib: ResourceDistrib ): String =
      show"${item.name.padTo( 32, ' ' )} P ${f"${distrib.pureNodes}% 2d"} " +
        show"N ${f"${distrib.normalNodes}% 2d"} I ${f"${distrib.impureNodes}% 2d"}"

    def showExtractorType( extractorType: ExtractorType, items: Map[ClassName, ResourceDistrib] ): String =
      items.toVector.map( (showItem _).tupled ).mkString_( show"${extractorType.description}\n  ", "\n  ", "" )

    Show.show(
      opts =>
        show"""NODES
              |${opts.resourceNodes.toVector.map( (showExtractorType _).tupled ).mkString_( "\n\n" )}
              |
              |WEIGHTS
              |${opts.resourceWeights.weights
                .map { case ( item, weight ) => show"$item: $weight" }
                .mkString( "\n" )}
              |""".stripMargin
    )
  }

  implicit val resourceOptionsEq: Eq[ResourceOptions] = Eq.by( ro => ( ro.resourceNodes, ro.resourceWeights ) )

  implicit val resourceOptionsDecoder: Decoder[ResourceOptions] = deriveDecoder[ResourceOptions]
  implicit val resourceOptionsEncoder: Encoder[ResourceOptions] = deriveEncoder[ResourceOptions]
}
