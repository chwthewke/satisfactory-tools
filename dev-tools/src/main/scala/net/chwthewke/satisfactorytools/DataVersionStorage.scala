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
  def docsFile: String
  val docsPath: Path = Path( docsKey )
}

object DataVersionStorage extends Enum[DataVersionStorage] {
  trait EarlyAccess { self: DataVersionStorage =>
    override def docsFile: String = "Docs.json"
  }

  trait Release { self: DataVersionStorage =>
    override def docsFile: String = "en-US.json"
  }

  case class GameSource( path: Path, ueVersionTag: Option[String] )

  val earlyAccessPath: Path  = Path( "E:\\EpicGames\\SatisfactoryEarlyAccess" )
  val experimentalPath: Path = Path( "E:\\EpicGames\\SatisfactoryExperimental" )

  case object Update4
      extends DataVersionStorage(
        None,
        "u4",
        ModelVersion( 1, "Satisfactory Update 4" )
      )
      with EarlyAccess
  case object Update5
      extends DataVersionStorage(
        None,
        "u5",
        ModelVersion( 2, "Satisfactory Update 5" )
      )
      with EarlyAccess
  case object Update6
      extends DataVersionStorage(
        None,
        "u6",
        ModelVersion( 3, "Satisfactory Update 6" )
      )
      with EarlyAccess
  case object Update7
      extends DataVersionStorage(
        None,
        "u7",
        ModelVersion( 4, "Satisfactory Update 7" )
      )
      with EarlyAccess
  case object Update8
      extends DataVersionStorage(
        None,
        "u8",
        ModelVersion( 5, "Satisfactory Update 8" )
      )
      with EarlyAccess
  case object Release1_0
      extends DataVersionStorage(
        Some( GameSource( earlyAccessPath, None ) ),
        "r1.0",
        ModelVersion( 6, "Satisfactory 1.0" )
      )
      with Release

  override val values: Vector[DataVersionStorage] = findValues.toVector
  implicit val dataVersionStorageOrder: Order[DataVersionStorage] =
    Order.by[DataVersionStorage, Int]( values.indexOf )
  implicit val dataVersionStorageOrdering: Ordering[DataVersionStorage] =
    Order.catsKernelOrderingForOrder
}
