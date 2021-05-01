package net.chwthewke.satisfactorytools
package web.view

import cats.syntax.show._
import scalatags.Text
import scalatags.Text.all._

import model.ExtractorType
import model.Item
import model.MapOptions
import model.Model
import model.ResourceDistrib
import model.ResourcePurity
import web.protocol.FormNames

object MapOptionsView {

  private def zipMap[K, A, B, C]( as: Map[K, A], bs: Map[K, B] )( combine: ( A, B ) => C ): Map[K, C] =
    as.keySet.intersect( bs.keySet ).map( k => ( k, combine( as( k ), bs( k ) ) ) ).toMap

  def view( model: Model, mapOptions: MapOptions ): Text.TypedTag[String] =
    div(
      zipMap( mapOptions.resourceNodes, model.defaultMapOptions.resourceNodes )(
        ( byEx, defaultByEx ) => zipMap( byEx, defaultByEx )( ( _, _ ) )
      ).toVector
        .sortBy { case ( ex, _ ) => ExtractorType.indexOf( ex ) }
        .map { case ( exT, nodes ) => viewNodes( exT, nodes.toVector ) }
    )

  // TODO default sets by start area?
  def viewNode(
      extractorType: ExtractorType,
      item: Item,
      resourceDistrib: ResourceDistrib,
      default: ResourceDistrib
  ): Text.TypedTag[String] =
    tr(
      td( item.displayName ),
      ResourcePurity.values.reverse
        .map( purity => td( nodeInput( extractorType, item, purity, resourceDistrib, default ) ) )
    )

  def nodeInput(
      extractorType: ExtractorType,
      item: Item,
      purity: ResourcePurity,
      resourceDistrib: ResourceDistrib,
      default: ResourceDistrib
  ): Text.TypedTag[String] =
    input(
      `type` := "number",
      name := FormNames.extractorItemPurityKey( extractorType, item, purity ),
      value := resourceDistrib.get( purity ),
      min := 0,
      max := default.get( purity )
    )

  def extractorTypeSectionHeader( extractorType: ExtractorType ): String = {
    val desc = extractorType.description.capitalize
    val note = if (extractorType == ExtractorType.WaterPump) " (pseudo-limit)" else ""
    show"$desc nodes$note"
  }

  def viewNodes(
      extractorType: ExtractorType,
      nodes: Vector[( Item, ( ResourceDistrib, ResourceDistrib ) )]
  ): Text.TypedTag[String] =
    fieldset(
      legend( extractorTypeSectionHeader( extractorType ) ),
      table(
        thead(
          tr( th( "Resource" ), th( "Pure" ), th( "Normal" ), th( "Impure" ) )
        ),
        tbody(
          nodes
            .sortBy { case ( it, _ ) => it.displayName }
            .map { case ( it, ( dist, default ) ) => viewNode( extractorType, it, dist, default ) }
        )
      )
    )
}
