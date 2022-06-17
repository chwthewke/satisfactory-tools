package net.chwthewke.satisfactorytools

import enumeratum.Enum
import enumeratum.EnumEntry
import java.nio.file.Path
import java.nio.file.Paths

import model.ModelVersion

sealed abstract class DataVersionStorage(
    val gameSource: Option[Path],
    val docsKey: String,
    val modelVersion: ModelVersion
) extends EnumEntry {
  val docsPath: Path = Paths.get( docsKey )
}

object DataVersionStorage extends Enum[DataVersionStorage] {
  val earlyAccessPath: Path  = Paths.get( "E:\\EpicGames\\SatisfactoryEarlyAccess" )
  val experimentalPath: Path = Paths.get( "E:\\EpicGames\\SatisfactoryExperimental" )

  case object Update4
      extends DataVersionStorage(
        None,
        "u4",
        ModelVersion( 1, "Satisfactory Update 4" )
      )
  case object Update5
      extends DataVersionStorage(
        Some( earlyAccessPath ),
        "u5",
        ModelVersion( 2, "Satisfactory Update 5" )
      )
  case object Update6
      extends DataVersionStorage(
        Some( experimentalPath ),
        "u6",
        ModelVersion( 3, "Satisfactory Update 6" )
      )

  override val values: IndexedSeq[DataVersionStorage] = findValues
}
