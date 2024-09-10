package net.chwthewke.satisfactorytools
package data

import cats.kernel.Order
import enumeratum.CatsEnum
import enumeratum.CirceEnum
import enumeratum.Enum
import enumeratum.EnumEntry

sealed abstract class SchematicType( override val entryName: String ) extends EnumEntry

object SchematicType extends Enum[SchematicType] with CirceEnum[SchematicType] with CatsEnum[SchematicType] {
  case object Alternate     extends SchematicType( "EST_Alternate" )
  case object Custom        extends SchematicType( "EST_Custom" )
  case object Customization extends SchematicType( "EST_Customization" )
  case object HardDrive     extends SchematicType( "EST_HardDrive" )
  case object Mam           extends SchematicType( "EST_MAM" )
  case object Milestone     extends SchematicType( "EST_Milestone" )
  case object Shop          extends SchematicType( "EST_ResourceSink" )
  case object Tutorial      extends SchematicType( "EST_Tutorial" )

  override val values: Vector[SchematicType] = findValues.toVector

  override implicit val eqInstance: Order[SchematicType]      = Order.by( values.indexOf )
  implicit val schematicTypeOrdering: Ordering[SchematicType] = Order.catsKernelOrderingForOrder
}
