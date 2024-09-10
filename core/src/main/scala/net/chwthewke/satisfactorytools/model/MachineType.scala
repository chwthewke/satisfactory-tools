package net.chwthewke.satisfactorytools
package model

import cats.Order
import cats.Show
import cats.syntax.show._
import enumeratum.Circe
import enumeratum.Enum
import enumeratum.EnumEntry
import io.circe.Decoder
import io.circe.Encoder

final case class MachineType( machineType: Either[ExtractorType, ManufacturerType] ) extends EnumEntry {
  def is( manufacturerType: ManufacturerType ): Boolean =
    machineType == Right( manufacturerType )

  def is( extractorType: ExtractorType ): Boolean =
    machineType == Left( extractorType )

  def isExtractor: Boolean = machineType.isLeft

  def extractorType: Option[ExtractorType] = machineType.left.toOption

  override def entryName: String = machineType.fold( _.entryName, _.entryName )
}

object MachineType extends Enum[MachineType] {

  def apply( extractorType: ExtractorType ): MachineType = MachineType( Left( extractorType ) )

  def apply( manufacturerType: ManufacturerType ): MachineType = MachineType( Right( manufacturerType ) )

  override val values: Vector[MachineType] =
    ExtractorType.values.map( MachineType( _ ) ) ++
      ManufacturerType.values.map( MachineType( _ ) )

  implicit val machineTypeShow: Show[MachineType]         = Show.show( _.machineType.fold( _.show, _.show ) )
  implicit val machineTypeOrder: Order[MachineType]       = Order.by( _.machineType )
  implicit val machineTypeOrdering: Ordering[MachineType] = Order.catsKernelOrderingForOrder

  implicit val machineTypeDecoder: Decoder[MachineType] = Circe.decoder( this )
  implicit val machineTypeEncoder: Encoder[MachineType] = Circe.encoder( this )
}
