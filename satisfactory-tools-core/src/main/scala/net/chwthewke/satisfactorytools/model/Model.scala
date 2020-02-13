package net.chwthewke.satisfactorytools.model

import cats.Monoid
import io.circe.Decoder

final case class Model(
    items: Map[ClassName, Item],
    extractors: Map[ClassName, Extractor],
    manufacturers: Map[ClassName, Manufacturer],
    recipes: Vector[Recipe[ClassName]]
)

object Model {
  val empty: Model = Model( Map.empty, Map.empty, Map.empty, Vector.empty )

  def items( items: Map[ClassName, Item] ): Model = Model( items, Map.empty, Map.empty, Vector.empty )
  def extractors( extractors: Map[ClassName, Extractor] ): Model =
    Model( Map.empty, extractors, Map.empty, Vector.empty )
  def manufacturers( manufacturers: Map[ClassName, Manufacturer] ): Model =
    Model( Map.empty, Map.empty, manufacturers, Vector.empty )
  def recipes( recipes: Vector[Recipe[ClassName]] ): Model =
    Model( Map.empty, Map.empty, Map.empty, recipes )

  implicit val modelMonoid: Monoid[Model] = new Monoid[Model] {
    override def empty: Model = Model.empty

    override def combine( x: Model, y: Model ): Model =
      Model(
        x.items ++ y.items,
        x.extractors ++ y.extractors,
        x.manufacturers ++ y.manufacturers,
        x.recipes ++ y.recipes
      )
  }

  private def decodeMap[A]( dec: Decoder[A] )( f: A => ClassName ): Decoder[Map[ClassName, A]] =
    Decoder.decodeVector( dec ).map( v => v.map( x => f( x ) -> x ).to( Map ) )

  def modelClassDecoder( nativeClass: NativeClass ): Decoder[Model] =
    nativeClass match {
      case NativeClass.`partDescClass` | NativeClass.`consumableDescClass` | NativeClass.`nuclearFuelDescClass` =>
        decodeMap( Item.itemDecoder( ItemType.Part ) )( _.className ).map( Model.items )
      case NativeClass.`biomassDescClass` =>
        decodeMap( Item.itemDecoder( ItemType.Biomass ) )( _.className ).map( Model.items )
      case NativeClass.`resourceDescClass` =>
        decodeMap( Item.itemDecoder( ItemType.Resource ) )( _.className ).map( Model.items )
      case NativeClass.`manufacturerDescClass` =>
        decodeMap( Decoder[Manufacturer] )( _.className ).map( Model.manufacturers )
      case NativeClass.`resourceExtractorClass` =>
        decodeMap( Decoder[Extractor] )( _.className ).map( Model.extractors )
      case NativeClass.`recipeClass` =>
        Decoder[Vector[Recipe[ClassName]]].map( Model.recipes )
      case _ => Decoder.const( Model.empty )
    }

  implicit val modelDecoder: Decoder[Model] =
    for {
      nativeClass <- Decoder[NativeClass].prepare( _.downField( "NativeClass" ) )
      model       <- modelClassDecoder( nativeClass ).prepare( _.downField( "Classes" ) )
    } yield model

}
