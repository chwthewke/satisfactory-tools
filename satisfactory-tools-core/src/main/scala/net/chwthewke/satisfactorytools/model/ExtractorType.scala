package net.chwthewke.satisfactorytools
package model

import cats.Eq
import cats.Show
import enumeratum.Enum
import enumeratum.EnumEntry

import data.ClassName
import data.Extractor

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

  def fromExtractor( extractor: Extractor ): Option[ExtractorType] =
    ExtractorType.values.find( _.dataKey.fold( _ == extractor.extractorTypeName, _ == extractor.className ) )

  implicit val extractorTypeShow: Show[ExtractorType] = Show( _.entryName )
  implicit val extractorTypeEq: Eq[ExtractorType]     = Eq.fromUniversalEquals

}
