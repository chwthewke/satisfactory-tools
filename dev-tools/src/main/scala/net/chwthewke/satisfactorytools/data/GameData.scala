package net.chwthewke.satisfactorytools
package data

import alleycats.std.iterable._
import cats.Monoid
import cats.Show
import cats.syntax.all._
import io.circe.Decoder

final case class GameData(
    items: Map[ClassName, ( Item, NativeClass )],
    extractors: Map[ClassName, Extractor],
    manufacturers: Map[ClassName, Manufacturer],
    powerGenerators: Map[ClassName, PowerGenerator],
    recipes: Vector[GameRecipe],
    schematics: Vector[Schematic],
    conveyorBelts: Vector[LogisticsData],
    pipelines: Vector[LogisticsData],
    buildingDescriptors: Map[ClassName, BuildingDescriptor]
) {
  def getBuildingIcon( className: ClassName ): Option[IconData] = {
    Option
      .when( className.name.startsWith( GameData.buildingPrefix ) )(
        GameData.descriptorPrefix + className.name.stripPrefix( GameData.buildingPrefix )
      )
      .map( ClassName( _ ) )
      .flatMap( buildingDescriptors.get )
      .flatMap( _.smallIcon )
  }

}

object GameData {
  private def init(
      items: Map[ClassName, ( Item, NativeClass )] = Map.empty,
      extractors: Map[ClassName, Extractor] = Map.empty,
      manufacturers: Map[ClassName, Manufacturer] = Map.empty,
      recipes: Vector[GameRecipe] = Vector.empty,
      powerGenerators: Map[ClassName, PowerGenerator] = Map.empty,
      schematics: Vector[Schematic] = Vector.empty,
      conveyorBelts: Vector[LogisticsData] = Vector.empty,
      pipelines: Vector[LogisticsData] = Vector.empty,
      buildingDescriptors: Map[ClassName, BuildingDescriptor] = Map.empty
  ): GameData =
    GameData(
      items,
      extractors,
      manufacturers,
      powerGenerators,
      recipes,
      schematics,
      conveyorBelts,
      pipelines,
      buildingDescriptors
    )

  val empty: GameData = init()

  def items( items: Map[ClassName, ( Item, NativeClass )] ): GameData        = init( items = items )
  def extractors( extractors: Map[ClassName, Extractor] ): GameData          = init( extractors = extractors )
  def manufacturers( manufacturers: Map[ClassName, Manufacturer] ): GameData = init( manufacturers = manufacturers )
  def recipes( recipes: Vector[GameRecipe] ): GameData                       = init( recipes = recipes )
  def nuclearGenerators( generators: Map[ClassName, PowerGenerator] ): GameData =
    init( powerGenerators = generators )
  def schematics( schematics: Vector[Schematic] ): GameData = init( schematics = schematics )
  def buildingDescriptors( descriptors: Map[ClassName, BuildingDescriptor] ): GameData =
    init( buildingDescriptors = descriptors )
  def conveyorBelts( logisticsData: Vector[LogisticsData] ): GameData = init( conveyorBelts = logisticsData )
  def pipelines( logisticsData: Vector[LogisticsData] ): GameData     = init( pipelines = logisticsData )

  implicit val gameDataMonoid: Monoid[GameData] = new Monoid[GameData] {
    override def empty: GameData = GameData.empty

    override def combine( x: GameData, y: GameData ): GameData =
      GameData(
        x.items ++ y.items,
        x.extractors ++ y.extractors,
        x.manufacturers ++ y.manufacturers,
        x.powerGenerators ++ y.powerGenerators,
        x.recipes ++ y.recipes,
        x.schematics ++ y.schematics,
        x.conveyorBelts ++ y.conveyorBelts,
        x.pipelines ++ y.pipelines,
        x.buildingDescriptors ++ y.buildingDescriptors
      )
  }

  import Parsers.ParserOps

  private def decodeMap[A]( dec: Decoder[A] )( f: A => ClassName ): Decoder[Map[ClassName, A]] =
    Decoder.decodeVector( dec ).map( v => v.map( x => f( x ) -> x ).to( Map ) )

  private val itemDecoder: Decoder[Item] =
    Decoder.forProduct6(
      "ClassName",
      "mDisplayName",
      "mForm",
      "mEnergyValue",
      "mResourceSinkPoints",
      "mSmallIcon"
    )( ( cn: ClassName, dn: String, fm: Form, ev: Double, pts: Option[Int], ico: IconData ) =>
      Item( cn, dn, fm, ev, pts.getOrElse( 0 ), ico )
    )(
      Decoder[ClassName],
      Decoder[String],
      Decoder[Form],
      Decoders.doubleStringDecoder,
      Decoder.decodeOption( Decoders.intStringDecoder ),
      Parsers.texture2d.decoder
    )

  def modelClassDecoder( nativeClass: NativeClass ): Decoder[GameData] =
    nativeClass match {
      case NativeClass.partDescClass | NativeClass.consumableDescClass | NativeClass.nuclearFuelDescClass |
          NativeClass.equipmentDescClass | NativeClass.biomassDescClass | NativeClass.resourceDescClass |
          NativeClass.ammoInstantDescClass | NativeClass.ammoInstantClassU6 | NativeClass.ammoProjDescClass |
          NativeClass.ammoProjClassU6 | NativeClass.ammoSpreadClassU6 | NativeClass.ammoColorDescClass |
          NativeClass.powerBoosterFuelClass | NativeClass.powerShardClass =>
        decodeMap( itemDecoder.tupleRight( nativeClass ) )( _._1.className ).map( GameData.items )
      case NativeClass.manufacturerClass =>
        decodeMap( Manufacturer.manufacturerDecoder( isCollider = false ) )( _.className ).map( GameData.manufacturers )
      case NativeClass.colliderClass =>
        decodeMap( Manufacturer.manufacturerDecoder( isCollider = true ) )( _.className ).map( GameData.manufacturers )
      case NativeClass.resourceExtractorClass | NativeClass.waterPumpClass | NativeClass.frackingExtractorClass =>
        decodeMap( Decoder[Extractor] )( _.className ).map( GameData.extractors )
      case NativeClass.recipeClass =>
        Decoder[Vector[GameRecipe]].map( GameData.recipes )
      case NativeClass.nuclearGeneratorClass | NativeClass.generatorClass =>
        decodeMap( Decoder[PowerGenerator] )( _.className ).map( GameData.nuclearGenerators )
      case NativeClass.schematicClass =>
        Decoder[Vector[Schematic]].map( GameData.schematics )
      case NativeClass.buildingDescriptorClass =>
        decodeMap( Decoder[BuildingDescriptor] )( _.className ).map( GameData.buildingDescriptors )
      case NativeClass.conveyorBeltClass =>
        Decoder.decodeVector( Decoder[LogisticsData.ConveyorBelt].map( _.data ) ).map( GameData.conveyorBelts )
      case NativeClass.pipelineClass =>
        Decoder.decodeVector( Decoder[LogisticsData.Pipeline].map( _.data ) ).map( GameData.pipelines )
      case _ => Decoder.const( GameData.empty )
    }

  implicit val gameDataDecoder: Decoder[GameData] =
    for {
      nativeClass <- Decoder[NativeClass].prepare( _.downField( "NativeClass" ) )
      gameData <-
        modelClassDecoder( nativeClass )
          .prepare( _.downField( "Classes" ) )
          .handleErrorWith( f => Decoder.failed( f.withMessage( show"in NativeClass $nativeClass: ${f.message}" ) ) )
    } yield gameData

  implicit val gameDataShow: Show[GameData] =
    Show( model => show"""Recipes:
                         |
                         |${model.recipes.map( _.show ).intercalate( "\n" )}
                         |
                         |
                         |Items:
                         |
                         |${model.items.values.map( _.show ).intercalate( "\n" )}
                         |
                         |
                         |Extractors:
                         |
                         |${model.extractors.values.map( _.show ).intercalate( "\n" )}
                         |
                         |Manufacturers:
                         |
                         |${model.manufacturers.values.map( _.show ).intercalate( "\n" )}
                         |
                         |""".stripMargin )

  private val buildingPrefix: String   = "Build_"
  private val descriptorPrefix: String = "Desc_"
}
