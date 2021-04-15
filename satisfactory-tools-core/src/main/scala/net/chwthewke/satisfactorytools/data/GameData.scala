package net.chwthewke.satisfactorytools
package data

import alleycats.std.iterable._
import cats.Monoid
import cats.Show
import cats.data.NonEmptyList
import cats.data.ValidatedNel
import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.option._
import cats.syntax.show._
import cats.syntax.traverse._
import cats.syntax.traverseFilter._
import io.circe.Decoder
import mouse.option._

import model.ClassName
import model.Countable
import model.Extractor
import model.Item
import model.ItemType
import model.Machine
import model.Manufacturer
import model.Model
import model.NativeClass
import model.Recipe
import model.ResourceDistrib

final case class GameData(
    items: Map[ClassName, Item],
    extractors: Map[ClassName, ( NativeClass, Extractor )],
    manufacturers: Map[ClassName, Manufacturer],
    recipes: Vector[Recipe[List[ClassName], ClassName]]
) {

  def toModel( map: MapConfig ): ValidatedNel[String, Model] = {
    val ( rawSelfExtraction, rawManufacturing ) = recipes.partition( isSelfExtraction )

    val extractionRecipes: ValidatedNel[String, Vector[( Item, Recipe[Machine, Item] )]] =
      rawSelfExtraction
        .traverse( validateRecipeItems )
        .map( getExtractionRecipes )

    val manufacturing: ValidatedNel[String, Vector[Recipe[Machine, Item]]] =
      rawManufacturing.traverseFilter( validateManufacturingRecipe )

    val resourceNodes: ValidatedNel[String, Map[Machine, Map[Item, ResourceDistrib]]] =
      extractors.toVector
        .flatTraverse {
          case ( _, ( extractorNativeClass, extractor ) ) =>
            validateResourceNodes( map, extractorNativeClass, extractor )
        }
        .map(
          flat => flat.groupMap( _._1 )( _._2 ).map { case ( ex, dists ) => ( Machine.extractor( ex ), dists.toMap ) }
        )

    ( extractionRecipes, manufacturing, resourceNodes )
      .mapN( ( ex, mf, rn ) => Model( mf, items, ex.map( _._1 ).distinct, ex, rn ) )
  }

  def validateItem( ccn: Countable[ClassName, Double] ): ValidatedNel[String, Countable[Item, Double]] =
    items
      .get( ccn.item )
      .toValidNel( show"Unknown item class ${ccn.item}" )
      .map( it => Countable( it, ccn.amount / it.form.simpleAmountFactor ) )

  def validateRecipeItems[A]( recipe: Recipe[A, ClassName] ): ValidatedNel[String, Recipe[A, Item]] =
    recipe
      .traverseIngredientsAndProducts( validateItem )
      .leftMap( errs => NonEmptyList.of( show"In ${recipe.displayName}\n  ${errs.intercalate( "\n  " )}" ) )

  def validateManufacturer( className: ClassName ): ValidatedNel[String, Machine] =
    manufacturers
      .get( className )
      .map( Machine.manufacturer )
      .toValidNel( show"Unknown machine class $className" )

  def validateManufacturingRecipe(
      recipe: Recipe[List[ClassName], ClassName]
  ): ValidatedNel[String, Option[Recipe[Machine, Item]]] =
    NonEmptyList
      .fromList( recipe.producedIn.filter( manufacturers.keySet ) )
      .traverse(
        ms =>
          (
            Option
              .when( ms.size == 1 )( ms.head )
              .toValidNel( show"Recipe ${recipe.displayName} is produced in multiple manufacturers" )
              .andThen( validateManufacturer ),
            validateRecipeItems( recipe )
          ).mapN( ( producer, itemRecipe ) => itemRecipe.copy( producedIn = producer ) )
      )

  def validateResourceNodes(
      map: MapConfig,
      extractorNativeClass: NativeClass,
      extractor: Extractor
  ): ValidatedNel[String, Vector[( Extractor, ( Item, ResourceDistrib ) )]] =
    map.resourceNodes
      .get( extractorNativeClass )
      .foldMap(
        byItem =>
          byItem.toVector.traverseFilter {
            case ( itemClass, dist ) =>
              items
                .get( itemClass )
                .toValidNel( show"Unknown item in map data: $itemClass" )
                .map( item => Option.when( canExtract( extractor, item ) )( ( extractor, ( item, dist ) ) ) )
          }
      )

  def isSelfExtraction[M, N]( recipe: Recipe[M, N] ): Boolean =
    recipe.ingredients == List( recipe.product.head )

  def getExtractionRecipes(
      selfExtraction: Vector[Recipe[List[ClassName], Item]]
  ): Vector[( Item, Recipe[Machine, Item] )] = {
    val ( converterExtractors, otherExtractors ) =
      extractors.values.toVector.partition( _._1 == NativeClass.resourceExtractorClass )

    (
      getConverterRecipes( converterExtractors.map( _._2 ), selfExtraction ) ++
        getOtherExtractionRecipes( otherExtractors.map( _._2 ) )
    ).map { case ( item, extractor ) => ( item, extractionRecipe( item, extractor ) ) }
  }

  def getConverterRecipes(
      converterExtractors: Vector[Extractor],
      selfExtraction: Vector[Recipe[List[ClassName], Item]]
  ): Vector[( Item, Extractor )] =
    for {
      selfExtractionRecipe <- selfExtraction
      item = selfExtractionRecipe.product.head.item
      converter <- convertersFor( converterExtractors, item )
    } yield ( item, converter )

  def getOtherExtractionRecipes(
      extractors: Vector[Extractor]
  ): Vector[( Item, Extractor )] =
    for {
      extractor        <- extractors
      allowedResources <- extractor.allowedResources.toVector
      resource         <- allowedResources.toList.toVector
      item             <- items.get( resource )
    } yield ( item, extractor )

  def convertersFor( converters: Vector[Extractor], item: Item ): Vector[Extractor] =
    converters.filter( canExtract( _, item ) )

  def canExtract( extractor: Extractor, item: Item ): Boolean =
    extractor.allowedResources.cata(
      _.contains_( item.className ),
      extractor.allowedResourceForms.contains( item.form )
    )

  def extractionRecipe( item: Item, extractor: Extractor ): Recipe[Machine, Item] =
    extractor.extractionRecipe( item )

}

object GameData {
  val empty: GameData = GameData( Map.empty, Map.empty, Map.empty, Vector.empty )

  def items( items: Map[ClassName, Item] ): GameData = GameData( items, Map.empty, Map.empty, Vector.empty )
  def extractors( extractors: Map[ClassName, ( NativeClass, Extractor )] ): GameData =
    GameData( Map.empty, extractors, Map.empty, Vector.empty )
  def manufacturers( manufacturers: Map[ClassName, Manufacturer] ): GameData =
    GameData( Map.empty, Map.empty, manufacturers, Vector.empty )
  def recipes( recipes: Vector[Recipe[List[ClassName], ClassName]] ): GameData =
    GameData( Map.empty, Map.empty, Map.empty, recipes )

  implicit val modelMonoid: Monoid[GameData] = new Monoid[GameData] {
    override def empty: GameData = GameData.empty

    override def combine( x: GameData, y: GameData ): GameData =
      GameData(
        x.items ++ y.items,
        x.extractors ++ y.extractors,
        x.manufacturers ++ y.manufacturers,
        x.recipes ++ y.recipes
      )
  }

  private def decodeMap[A]( dec: Decoder[A] )( f: A => ClassName ): Decoder[Map[ClassName, A]] =
    Decoder.decodeVector( dec ).map( v => v.map( x => f( x ) -> x ).to( Map ) )

  def modelClassDecoder( nativeClass: NativeClass ): Decoder[GameData] =
    nativeClass match {
      case NativeClass.`partDescClass` | NativeClass.`consumableDescClass` | NativeClass.`nuclearFuelDescClass` =>
        decodeMap( Item.itemDecoder( ItemType.Part ) )( _.className ).map( GameData.items )
      case NativeClass.`equipmentDescClass` =>
        decodeMap( Item.itemDecoder( ItemType.Equipment ) )( _.className ).map( GameData.items )
      case NativeClass.`biomassDescClass` =>
        decodeMap( Item.itemDecoder( ItemType.Biomass ) )( _.className ).map( GameData.items )
      case NativeClass.`resourceDescClass` =>
        decodeMap( Item.itemDecoder( ItemType.Resource ) )( _.className ).map( GameData.items )
      case NativeClass.`manufacturerDescClass` =>
        decodeMap( Decoder[Manufacturer] )( _.className ).map( GameData.manufacturers )
      case NativeClass.`resourceExtractorClass` | NativeClass.`waterPumpClass` | NativeClass.`frackingExtractorClass` =>
        decodeMap( Decoder[Extractor] )( _.className )
          .map( exs => GameData.extractors( exs.map { case ( c, ex ) => ( c, ( nativeClass, ex ) ) } ) )
      case NativeClass.`recipeClass` =>
        Decoder[Vector[Recipe[List[ClassName], ClassName]]].map( GameData.recipes )
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

  implicit val protoModelShow: Show[GameData] = Show(
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
