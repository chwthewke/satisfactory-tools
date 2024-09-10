package net.chwthewke.satisfactorytools
package model

import cats.Order
import cats.Show
import enumeratum.Circe
import enumeratum.Enum
import enumeratum.EnumEntry
import io.circe.Decoder
import io.circe.Encoder
import io.circe.KeyDecoder
import io.circe.KeyEncoder

import data.ClassName

abstract sealed class ExtractorType(
    override val entryName: String,
    val description: String,
    val dataKey: Either[String, ClassName]
) extends EnumEntry

object ExtractorType extends Enum[ExtractorType] {
  private val waterExtractorClass: ClassName    = ClassName( "Build_WaterPump_C" )
  private val oilExtractorClass: ClassName      = ClassName( "Build_OilPump_C" )
  private val frackingExtractorClass: ClassName = ClassName( "Build_FrackingExtractor_C" )

  final case object Miner     extends ExtractorType( "miner", "miner", Left( "Miner" ) )
  final case object WaterPump extends ExtractorType( "water-pump", "water extractor", Right( waterExtractorClass ) )
  final case object OilPump   extends ExtractorType( "oil-pump", "oil extractor", Right( oilExtractorClass ) )
  final case object FrackingExtractor
      extends ExtractorType( "fracking", "fracking extractor", Right( frackingExtractorClass ) )

  override val values: Vector[ExtractorType] = findValues.toVector

  implicit val extractorTypeShow: Show[ExtractorType]         = Show( _.entryName )
  implicit val extractorTypeOrder: Order[ExtractorType]       = Order.by( values.indexOf )
  implicit val extractorTypeOrdering: Ordering[ExtractorType] = Order.catsKernelOrderingForOrder

  implicit val extractorTypeDecoder: Decoder[ExtractorType] = Circe.decoder( this )
  implicit val extractorTypeEncoder: Encoder[ExtractorType] = Circe.encoder( this )

  implicit val extractorTypeKeyDecoder: KeyDecoder[ExtractorType] = Circe.keyDecoder( this )
  implicit val extractorTypeKeyEncoder: KeyEncoder[ExtractorType] = Circe.keyEncoder( this )
}
