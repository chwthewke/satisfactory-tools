package net.chwthewke.satisfactorytools
package model

import cats.Show
import cats.instances.string._
import cats.syntax.contravariant._
import io.circe.Decoder

final case class NativeClass( value: String ) extends AnyVal

object NativeClass {
  implicit val nativeClassDecoder: Decoder[NativeClass] = Decoder[String].map( NativeClass( _ ) )

  implicit val nativeClassShow: Show[NativeClass] = Show[String].contramap( _.value )

  val biomassDescClass: NativeClass       = NativeClass( "Class'/Script/FactoryGame.FGItemDescriptorBiomass'" )
  val consumableDescClass: NativeClass    = NativeClass( "Class'/Script/FactoryGame.FGConsumableDescriptor'" )
  val nuclearFuelDescClass: NativeClass   = NativeClass( "Class'/Script/FactoryGame.FGItemDescriptorNuclearFuel'" )
  val partDescClass: NativeClass          = NativeClass( "Class'/Script/FactoryGame.FGItemDescriptor'" )
  val recipeClass: NativeClass            = NativeClass( "Class'/Script/FactoryGame.FGRecipe'" )
  val resourceDescClass: NativeClass      = NativeClass( "Class'/Script/FactoryGame.FGResourceDescriptor'" )
  val manufacturerDescClass: NativeClass  = NativeClass( "Class'/Script/FactoryGame.FGBuildableManufacturer'" )
  val resourceExtractorClass: NativeClass = NativeClass( "Class'/Script/FactoryGame.FGBuildableResourceExtractor'" )
}
