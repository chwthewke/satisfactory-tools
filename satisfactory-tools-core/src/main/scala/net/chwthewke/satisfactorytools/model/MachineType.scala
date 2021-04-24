package net.chwthewke.satisfactorytools
package model

sealed abstract class MachineType( val name: String, val extractorType: Option[ExtractorType] ) {
  def isExtractor: Boolean = extractorType.isDefined
}

object MachineType {

  final case object Manufacturer extends MachineType( "Manufacturer", None )
  final case class Extractor( override val name: String, override val extractorType: Option[ExtractorType] )
      extends MachineType( name, extractorType )

  object Extractor {
    def apply( extractorType: ExtractorType ): Extractor = Extractor( extractorType.entryName, Some( extractorType ) )
  }

  val values: Vector[MachineType] = Manufacturer +: ExtractorType.values.map( Extractor( _ ) )

}
