package net.chwthewke.satisfactorytools
package data

import alleycats.std.iterable._
import alleycats.std.map._
import cats.Id
import cats.Monoid
import cats.Order.catsKernelOrderingForOrder
import cats.Show
import cats.data.NonEmptyList
import cats.data.ValidatedNel
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.show._
import cats.syntax.traverse._
import cats.syntax.traverseFilter._
import io.circe.Decoder
import mouse.option._
import scala.collection.immutable.SortedMap

import model.ClassName
import model.Countable
import model.Extractor
import model.ExtractorType
import model.Item
import model.ItemType
import model.Machine
import model.MachineType
import model.Manufacturer
import model.ResourceOptions
import model.Model
import model.NativeClass
import model.Recipe
import net.chwthewke.satisfactorytools.model.ResourcePurity

final case class GameData(
    items: Map[ClassName, Item],
    extractors: Map[ClassName, Extractor],
    manufacturers: Map[ClassName, Manufacturer],
    recipes: Vector[Recipe[List[ClassName], ClassName]]
) {

  def toModel( mapConfig: MapConfig ): ValidatedNel[String, Model] = {
    val ( rawSelfExtraction, rawManufacturing ) = recipes.partition( isSelfExtraction )

    val extractorMachines = extractors.traverse( ex => Machine.extractor( ex ).toValidatedNel.tupleLeft( ex ) )

    val extractionRecipes: ValidatedNel[String, Vector[( Item, ResourcePurity, Recipe[Machine, Item] )]] =
      ( extractorMachines, rawSelfExtraction.traverse( validateRecipeItems ) )
        .mapN( getExtractionRecipes )

    val manufacturing: ValidatedNel[String, Vector[Recipe[Machine, Item]]] =
      rawManufacturing.traverseFilter( validateManufacturingRecipe )

    val defaultResourceOptions: ValidatedNel[String, ResourceOptions] =
      ResourceOptions.init( items, mapConfig ).toValidatedNel

    ( extractionRecipes, manufacturing, defaultResourceOptions )
      .mapN( ( ex, mf, ro ) => Model( mf, items.to( SortedMap ), ex.map( _._1 ).distinct, ex, ro ) )
  }

  def validateItem( ccn: Countable[Double, ClassName] ): ValidatedNel[String, Countable[Double, Item]] =
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

  def isSelfExtraction[M, N]( recipe: Recipe[M, N] ): Boolean =
    recipe.ingredients == List( recipe.products.head )

  def getExtractionRecipes(
      machines: Map[ClassName, ( Extractor, Machine )],
      selfExtraction: Vector[Recipe[List[ClassName], Item]]
  ): Vector[( Item, ResourcePurity, Recipe[Machine, Item] )] = {
    val ( miners, otherExtractors ) =
      machines.values.toVector.partition( _._2.machineType == MachineType.Extractor( ExtractorType.Miner ) )

    (
      getConverterRecipes( miners, selfExtraction ) ++
        getOtherExtractionRecipes( otherExtractors )
    ).flatMap {
      case ( item, extractor, machine ) =>
        ResourcePurity.values
          .map( purity => ( item, purity, extractionRecipe( item, extractor, purity, machine ) ) )
    }
  }

  def getConverterRecipes(
      converterExtractors: Vector[( Extractor, Machine )],
      selfExtraction: Vector[Recipe[List[ClassName], Item]]
  ): Vector[( Item, Extractor, Machine )] =
    for {
      selfExtractionRecipe <- selfExtraction
      item = selfExtractionRecipe.products.head.item
      ( extractor, machine ) <- converterExtractors
    } yield ( item, extractor, machine )

  def getOtherExtractionRecipes(
      extractors: Vector[( Extractor, Machine )]
  ): Vector[( Item, Extractor, Machine )] =
    for {
      ( extractor, machine ) <- extractors
      allowedResources       <- extractor.allowedResources.toVector
      resource               <- allowedResources.toList.toVector
      item                   <- items.get( resource )
    } yield ( item, extractor, machine )

  def convertersFor( converters: Vector[( Extractor, Machine )], item: Item ): Vector[( Extractor, Machine )] =
    converters.filter { case ( ex, _ ) => canExtract( ex, item ) }

  def canExtract( extractor: Extractor, item: Item ): Boolean =
    extractor.allowedResources.cata(
      _.contains_( item.className ),
      extractor.allowedResourceForms.contains( item.form )
    )

  def extractionRecipe(
      item: Item,
      extractor: Extractor,
      purity: ResourcePurity,
      machine: Machine
  ): Recipe[Machine, Item] =
    Recipe(
      ClassName( show"${item.className}_${extractor.className}" ),
      show"${item.displayName} ($purity, ${extractor.displayName})",
      Nil,
      NonEmptyList.of( Countable( item, extractor.itemsPerCycle.toDouble / item.form.simpleAmountFactor ) ),
      extractor.cycleTime,
      machine
    ).traverseIngredientsAndProducts[Id, Item] {
      case Countable( item, amount ) => Countable( item, amount * purity.multiplier )
    }

}

object GameData {
  val empty: GameData = GameData( Map.empty, Map.empty, Map.empty, Vector.empty )

  def items( items: Map[ClassName, Item] ): GameData = GameData( items, Map.empty, Map.empty, Vector.empty )
  def extractors( extractors: Map[ClassName, Extractor] ): GameData =
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
        decodeMap( Decoder[Extractor] )( _.className ).map( GameData.extractors )
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
