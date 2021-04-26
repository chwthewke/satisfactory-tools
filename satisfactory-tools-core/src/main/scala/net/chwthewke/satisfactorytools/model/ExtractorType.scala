package net.chwthewke.satisfactorytools
package model

import enumeratum.Enum
import enumeratum.EnumEntry

abstract sealed class ExtractorType( override val entryName: String, val description: String, val dataKey: String )
    extends EnumEntry

object ExtractorType extends Enum[ExtractorType] {
  private val waterExtractorClass: ClassName    = ClassName( "Build_WaterPump_C" )
  private val oilExtractorClass: ClassName      = ClassName( "Build_OilPump_C" )
  private val frackingExtractorClass: ClassName = ClassName( "Build_FrackingExtractor_C" )

  final case object Miner     extends ExtractorType( "miner", "miner", "Miner" )
  final case object WaterPump extends ExtractorType( "water-pump", "water extractor", waterExtractorClass.name )
  final case object OilPump   extends ExtractorType( "oil-pump", "oil extractor", oilExtractorClass.name )
  final case object FrackingExtractor
      extends ExtractorType( "fracking", "fracking extractor", frackingExtractorClass.name )

  override val values: Vector[ExtractorType] = findValues.toVector

  def fromExtractor( extractor: Extractor ): Option[ExtractorType] =
    ExtractorType.values.find( t => t.dataKey == extractor.extractorTypeName || t.dataKey == extractor.className.name )

}
