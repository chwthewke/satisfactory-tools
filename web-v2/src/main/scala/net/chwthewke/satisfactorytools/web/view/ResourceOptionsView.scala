package net.chwthewke.satisfactorytools
package web.view

import cats.syntax.all._
import scalatags.Text
import scalatags.Text.Tag

import data.Item
import model.ExtractorType
import model.Model
import model.ResourceDistrib
import model.ResourceOptions
import model.ResourcePurity
import model.ResourceWeights
import web.forms._

object ResourceOptionsView extends ( ( Model, ResourceOptions ) => Tag ) {
  import Text.all._

  private def zipMap[K, A, B, C]( as: Map[K, A], bs: Map[K, B] )( combine: ( A, B ) => C ): Map[K, C] =
    as.keySet.intersect( bs.keySet ).map( k => ( k, combine( as( k ), bs( k ) ) ) ).toMap

  override def apply( model: Model, resourceOptions: ResourceOptions ): Tag =
    div(
      viewResourceWeights( model, resourceOptions.resourceWeights ),
      zipMap( resourceOptions.resourceNodes, model.defaultResourceOptions.resourceNodes )( ( byEx, defaultByEx ) =>
        zipMap( byEx, defaultByEx )( ( _, _ ) )
      ).toVector
        .sortBy( _._1 )
        .map {
          case ( exT, nodes ) =>
            viewNodes(
              exT,
              nodes.toVector
                .flatMap { case ( itemClass, v ) => model.items.get( itemClass ).tupleRight( v ) }
            )
        }
    )

  // TODO default sets by start area?
  def viewNode(
      extractorType: ExtractorType,
      item: Item,
      resourceDistrib: ResourceDistrib,
      default: ResourceDistrib
  ): Tag =
    tr(
      td( item.displayName ),
      ResourcePurity.values
        .map( purity => td( nodeInput( extractorType, item, purity, resourceDistrib, default ) ) )
    )

  def nodeInput(
      extractorType: ExtractorType,
      item: Item,
      purity: ResourcePurity,
      resourceDistrib: ResourceDistrib,
      default: ResourceDistrib
  ): Tag =
    input(
      `type` := "number",
      name   := Keys.extractorItemPurityKey( extractorType, item, purity ),
      value  := resourceDistrib.get( purity ),
      min    := 0,
      max    := default.get( purity )
    )

  def extractorTypeSectionHeader( extractorType: ExtractorType ): String = {
    val desc = extractorType.description.capitalize
    val note = if (extractorType == ExtractorType.WaterPump) " (pseudo-limit)" else ""
    show"$desc nodes$note"
  }

  def viewNodes(
      extractorType: ExtractorType,
      nodes: Vector[( Item, ( ResourceDistrib, ResourceDistrib ) )]
  ): Tag =
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

  def viewResourceWeights( model: Model, weights: ResourceWeights ): Tag =
    fieldset(
      legend( "Resource weight tweaks" ),
      table(
        thead(
          tr(
            th( textAlign.left, "Resource" ),
            th( textAlign.left, "Use more" ),
            th( textAlign.right, "Use less" )
          )
        ),
        tbody(
          model.extractedItems
            .sortBy( _.displayName )
            .map( item =>
              tr(
                td( item.displayName ),
                td(
                  colspan := 2,
                  input(
                    `type` := "range",
                    name   := Keys.resourceWeightKey( item ),
                    min    := 0,
                    max    := ( 2 * ResourceWeights.range ),
                    step   := 1,
                    value  := weights.weights.getOrElse( item.className, ResourceWeights.range )
                  )
                )
              )
            )
        )
      )
    )

}
