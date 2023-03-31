package net.chwthewke.satisfactorytools
package model

import cats.Order
import enumeratum.CatsEnum
import enumeratum.CirceEnum
import enumeratum.Enum
import enumeratum.EnumEntry

sealed abstract class ResearchCategory( val keys: Vector[String] ) extends EnumEntry

object ResearchCategory
    extends Enum[ResearchCategory]
    with CatsEnum[ResearchCategory]
    with CirceEnum[ResearchCategory] {
  case object AlienOrganisms extends ResearchCategory( Vector( "AO", "ACarapace", "AOrgans" ) )
  case object PowerSlugs     extends ResearchCategory( Vector( "PowerSlugs" ) )
  case object Nutrients      extends ResearchCategory( Vector( "Nutrients" ) )
  case object XMas           extends ResearchCategory( Vector( "XMas" ) )
  case object Quartz         extends ResearchCategory( Vector( "Quartz" ) )
  case object Caterium       extends ResearchCategory( Vector( "Caterium" ) )
  case object Sulfur         extends ResearchCategory( Vector( "Sulfur" ) )
  case object Mycelia        extends ResearchCategory( Vector( "Mycelia" ) )
  case object FlowerPetals   extends ResearchCategory( Vector( "FlowerPetals" ) )

  override val values: Vector[ResearchCategory]             = findValues.toVector
  override implicit val eqInstance: Order[ResearchCategory] = Order.by( values.indexOf )
}
