package net.chwthewke.satisfactorytools
package data

import cats.Order
import cats.Show
import cats.derived.semiauto
import cats.syntax.all._
import io.circe.Decoder

final case class NativeClass( value: String ) extends AnyVal

object NativeClass {
  implicit val nativeClassDecoder: Decoder[NativeClass] =
    Decoder[String].map( str => NativeClass( str.stripPrefix( "/Script/CoreUObject." ) ) )

  implicit val nativeClassShow: Show[NativeClass]         = Show[String].contramap( _.value )
  implicit val nativeClassOrder: Order[NativeClass]       = semiauto.order[NativeClass]
  implicit val nativeClassOrdering: Ordering[NativeClass] = Order.catsKernelOrderingForOrder

  val biomassDescClass: NativeClass     = NativeClass( "Class'/Script/FactoryGame.FGItemDescriptorBiomass'" )
  val consumableDescClass: NativeClass  = NativeClass( "Class'/Script/FactoryGame.FGConsumableDescriptor'" )
  val equipmentDescClass: NativeClass   = NativeClass( "Class'/Script/FactoryGame.FGEquipmentDescriptor'" )
  val nuclearFuelDescClass: NativeClass = NativeClass( "Class'/Script/FactoryGame.FGItemDescriptorNuclearFuel'" )
  val partDescClass: NativeClass        = NativeClass( "Class'/Script/FactoryGame.FGItemDescriptor'" )
  val ammoProjDescClass: NativeClass    = NativeClass( "Class'/Script/FactoryGame.FGItemDescAmmoTypeProjectile'" )
  val ammoProjClassU6: NativeClass      = NativeClass( "Class'/Script/FactoryGame.FGAmmoTypeProjectile'" )
  val ammoSpreadClassU6: NativeClass    = NativeClass( "Class'/Script/FactoryGame.FGAmmoTypeSpreadshot'" )
  val ammoInstantDescClass: NativeClass = NativeClass( "Class'/Script/FactoryGame.FGItemDescAmmoTypeInstantHit'" )
  val ammoInstantClassU6: NativeClass   = NativeClass( "Class'/Script/FactoryGame.FGAmmoTypeInstantHit'" )
  val ammoColorDescClass: NativeClass   = NativeClass( "Class'/Script/FactoryGame.FGItemDescAmmoTypeColorCartridge'" )
  val recipeClass: NativeClass          = NativeClass( "Class'/Script/FactoryGame.FGRecipe'" )
  val resourceDescClass: NativeClass    = NativeClass( "Class'/Script/FactoryGame.FGResourceDescriptor'" )
  val manufacturerClass: NativeClass    = NativeClass( "Class'/Script/FactoryGame.FGBuildableManufacturer'" )
  val colliderClass: NativeClass  = NativeClass( "Class'/Script/FactoryGame.FGBuildableManufacturerVariablePower'" )
  val generatorClass: NativeClass = NativeClass( "Class'/Script/FactoryGame.FGBuildableGeneratorFuel'" )
  val nuclearGeneratorClass: NativeClass  = NativeClass( "Class'/Script/FactoryGame.FGBuildableGeneratorNuclear'" )
  val resourceExtractorClass: NativeClass = NativeClass( "Class'/Script/FactoryGame.FGBuildableResourceExtractor'" )
  val waterPumpClass: NativeClass         = NativeClass( "Class'/Script/FactoryGame.FGBuildableWaterPump'" )
  val frackingExtractorClass: NativeClass = NativeClass( "Class'/Script/FactoryGame.FGBuildableFrackingExtractor'" )
  val schematicClass: NativeClass         = NativeClass( "Class'/Script/FactoryGame.FGSchematic'" )
  val powerBoosterFuelClass: NativeClass  = NativeClass( "Class'/Script/FactoryGame.FGItemDescriptorPowerBoosterFuel'" )
  val powerShardClass: NativeClass        = NativeClass( "Class'/Script/FactoryGame.FGPowerShardDescriptor'" )
  val buildingDescriptorClass: NativeClass = NativeClass( "Class'/Script/FactoryGame.FGBuildingDescriptor'" )
  val conveyorBeltClass: NativeClass       = NativeClass( "Class'/Script/FactoryGame.FGBuildableConveyorBelt'" )
  val pipelineClass: NativeClass           = NativeClass( "Class'/Script/FactoryGame.FGBuildablePipeline'" )
}
