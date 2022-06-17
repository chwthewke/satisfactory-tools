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
    nuclearGenerators: Map[ClassName, NuclearGenerator]
)

object GameData {
  val empty: GameData = GameData( Map.empty, Map.empty, Map.empty, Vector.empty, Map.empty )

  def items( items: Map[ClassName, Item] ): GameData = GameData( items, Map.empty, Map.empty, Vector.empty, Map.empty )
  def extractors( extractors: Map[ClassName, Extractor] ): GameData =
    GameData( Map.empty, extractors, Map.empty, Vector.empty, Map.empty )
  def manufacturers( manufacturers: Map[ClassName, Manufacturer] ): GameData =
    GameData( Map.empty, Map.empty, manufacturers, Vector.empty, Map.empty )
  def recipes( recipes: Vector[GameRecipe] ): GameData =
    GameData( Map.empty, Map.empty, Map.empty, recipes, Map.empty )
  def nuclearGenerators( generators: Map[ClassName, NuclearGenerator] ): GameData =
    GameData( Map.empty, Map.empty, Map.empty, Vector.empty, generators )

  implicit val modelMonoid: Monoid[GameData] = new Monoid[GameData] {
    override def empty: GameData = GameData.empty

    override def combine( x: GameData, y: GameData ): GameData =
      GameData(
        x.items ++ y.items,
        x.extractors ++ y.extractors,
        x.manufacturers ++ y.manufacturers,
        x.recipes ++ y.recipes,
        x.nuclearGenerators ++ y.nuclearGenerators
      )
  }

  private def decodeMap[A]( dec: Decoder[A] )( f: A => ClassName ): Decoder[Map[ClassName, A]] =
    Decoder.decodeVector( dec ).map( v => v.map( x => f( x ) -> x ).to( Map ) )

  def modelClassDecoder( nativeClass: NativeClass ): Decoder[GameData] =
    nativeClass match {
      case NativeClass.partDescClass | NativeClass.consumableDescClass | NativeClass.nuclearFuelDescClass |
          NativeClass.equipmentDescClass | NativeClass.biomassDescClass | NativeClass.resourceDescClass |
          NativeClass.ammoInstantDescClass | NativeClass.ammoInstantClassU6 | NativeClass.ammoProjDescClass |
          NativeClass.ammoProjClassU6 | NativeClass.ammoSpreadClassU6 | NativeClass.ammoColorDescClass =>
        decodeMap( Decoder[Item] )( _.className ).map( GameData.items )
      case NativeClass.manufacturerClass | NativeClass.colliderClass =>
        decodeMap( Decoder[Manufacturer] )( _.className ).map( GameData.manufacturers )
      case NativeClass.resourceExtractorClass | NativeClass.waterPumpClass | NativeClass.frackingExtractorClass =>
        decodeMap( Decoder[Extractor] )( _.className ).map( GameData.extractors )
      case NativeClass.recipeClass =>
        Decoder[Vector[GameRecipe]].map( GameData.recipes )
      case NativeClass.nuclearGeneratorClass =>
        decodeMap( Decoder[NuclearGenerator] )( _.className ).map( GameData.nuclearGenerators )
      case _ => Decoder.const( GameData.empty )
    }

  implicit val modelDecoder: Decoder[GameData] =
    for {
      nativeClass <- Decoder[NativeClass].prepare( _.downField( "NativeClass" ) )
      model <- modelClassDecoder( nativeClass )
                .prepare( _.downField( "Classes" ) )
                .handleErrorWith(
                  f => Decoder.failed( f.withMessage( show"in NativeClass $nativeClass: ${f.message}" ) )
                )
    } yield model

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
