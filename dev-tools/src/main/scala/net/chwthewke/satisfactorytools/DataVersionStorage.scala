package net.chwthewke.satisfactorytools

import cats.Order
import enumeratum.Enum
import enumeratum.EnumEntry
import fs2.io.file.Path

import model.ModelVersion

sealed abstract class DataVersionStorage(
    val gameSource: Option[DataVersionStorage.GameSource],
    val docsKey: String,
    val modelVersion: ModelVersion
) extends EnumEntry {
  val docsPath: Path = Path( docsKey )
}

object DataVersionStorage extends Enum[DataVersionStorage] {
  case class GameSource( path: Path, ueVersionTag: String )

  val earlyAccessPath: Path  = Path( "E:\\EpicGames\\SatisfactoryEarlyAccess" )
  val experimentalPath: Path = Path( "E:\\EpicGames\\SatisfactoryExperimental" )

  case object Update4
      extends DataVersionStorage(
        None,
        "u4",
        ModelVersion( 1, "Satisfactory Update 4" )
      )
  case object Update5
      extends DataVersionStorage(
        Some( GameSource( earlyAccessPath, "ue4.26" ) ),
        "u5",
        ModelVersion( 2, "Satisfactory Update 5" )
      )
  case object Update6
      extends DataVersionStorage(
        Some( GameSource( experimentalPath, "ue4.26" ) ),
        "u6",
        ModelVersion( 3, "Satisfactory Update 6" )
      )

  override val values: Vector[DataVersionStorage] = findValues.toVector
  implicit val dataVersionStorageOrder: Order[DataVersionStorage] =
    Order.by[DataVersionStorage, Int]( values.indexOf )
}
