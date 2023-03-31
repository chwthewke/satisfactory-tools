package net.chwthewke.satisfactorytools
package data

import alleycats.std.iterable._
import cats.Monoid
import cats.Show
import cats.syntax.foldable._
import cats.syntax.show._
import io.circe.Decoder

final case class GameData(
    items: Map[ClassName, Item],
    extractors: Map[ClassName, Extractor],
    manufacturers: Map[ClassName, Manufacturer],
    recipes: Vector[GameRecipe],
    nuclearGenerators: Map[ClassName, NuclearGenerator],
    schematics: Vector[Schematic]
)

object GameData {
  private def init(
      items: Map[ClassName, Item] = Map.empty,
      extractors: Map[ClassName, Extractor] = Map.empty,
      manufacturers: Map[ClassName, Manufacturer] = Map.empty,
      recipes: Vector[GameRecipe] = Vector.empty,
      nuclearGenerators: Map[ClassName, NuclearGenerator] = Map.empty,
      schematics: Vector[Schematic] = Vector.empty
  ): GameData =
    GameData( items, extractors, manufacturers, recipes, nuclearGenerators, schematics )

  val empty: GameData = init()

  def items( items: Map[ClassName, Item] ): GameData                         = init( items = items )
  def extractors( extractors: Map[ClassName, Extractor] ): GameData          = init( extractors = extractors )
  def manufacturers( manufacturers: Map[ClassName, Manufacturer] ): GameData = init( manufacturers = manufacturers )
  def recipes( recipes: Vector[GameRecipe] ): GameData                       = init( recipes = recipes )
  def nuclearGenerators( generators: Map[ClassName, NuclearGenerator] ): GameData =
    init( nuclearGenerators = generators )
  def schematics( schematics: Vector[Schematic] ): GameData = init( schematics = schematics )

  implicit val gameDataMonoid: Monoid[GameData] = new Monoid[GameData] {
    override def empty: GameData = GameData.empty

    override def combine( x: GameData, y: GameData ): GameData =
      GameData(
        x.items ++ y.items,
        x.extractors ++ y.extractors,
        x.manufacturers ++ y.manufacturers,
        x.recipes ++ y.recipes,
        x.nuclearGenerators ++ y.nuclearGenerators,
        x.schematics ++ y.schematics
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
    )(
      ( cn: ClassName, dn: String, fm: Form, ev: Double, pts: Option[Int], ico: IconData ) =>
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
          NativeClass.ammoProjClassU6 | NativeClass.ammoSpreadClassU6 | NativeClass.ammoColorDescClass =>
        decodeMap( itemDecoder )( _.className ).map( GameData.items )
      case NativeClass.manufacturerClass | NativeClass.colliderClass =>
        decodeMap( Decoder[Manufacturer] )( _.className ).map( GameData.manufacturers )
      case NativeClass.resourceExtractorClass | NativeClass.waterPumpClass | NativeClass.frackingExtractorClass =>
        decodeMap( Decoder[Extractor] )( _.className ).map( GameData.extractors )
      case NativeClass.recipeClass =>
        Decoder[Vector[GameRecipe]].map( GameData.recipes )
      case NativeClass.nuclearGeneratorClass =>
        decodeMap( Decoder[NuclearGenerator] )( _.className ).map( GameData.nuclearGenerators )
      case NativeClass.schematicClass =>
        Decoder[Vector[Schematic]].map( GameData.schematics )
      case _ => Decoder.const( GameData.empty )
    }

  implicit val gameDataDecoder: Decoder[GameData] =
    for {
      nativeClass <- Decoder[NativeClass].prepare( _.downField( "NativeClass" ) )
      gameData <- modelClassDecoder( nativeClass )
                   .prepare( _.downField( "Classes" ) )
                   .handleErrorWith(
                     f => Decoder.failed( f.withMessage( show"in NativeClass $nativeClass: ${f.message}" ) )
                   )
    } yield gameData

  implicit val gameDataShow: Show[GameData] = Show(
    model => show"""Recipes:
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
                   |""".stripMargin
  )
}
