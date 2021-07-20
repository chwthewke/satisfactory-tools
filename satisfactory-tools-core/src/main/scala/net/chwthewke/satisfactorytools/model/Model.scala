package net.chwthewke.satisfactorytools
package model

import alleycats.std.iterable._
import alleycats.std.map._
import cats.Id
import cats.Order.catsKernelOrderingForOrder
import cats.Show
import cats.Traverse
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
import mouse.option._
import scala.collection.immutable.SortedMap
import scala.concurrent.duration._

import data.ClassName
import data.Countable
import data.Extractor
import data.GameData
import data.GameRecipe
import data.Item

case class Model(
    manufacturingRecipes: Vector[Recipe],
    items: SortedMap[ClassName, Item],
    extractedItems: Vector[Item],
    extractionRecipes: Vector[( Item, ResourcePurity, Recipe )],
    defaultResourceOptions: ResourceOptions
)

object Model {

  def init( data: GameData, mapConfig: MapConfig ): ValidatedNel[String, Model] = {
    val ( rawSelfExtraction, rawManufacturing ) = data.recipes.partition( isSelfExtraction )

    val extractorMachines = data.extractors.traverse( ex => Machine.extractor( ex ).toValidatedNel.tupleLeft( ex ) )

    val extractionRecipes: ValidatedNel[String, Vector[( Item, ResourcePurity, Recipe )]] =
      extractorMachines.andThen( extractors => getExtractionRecipes( data, extractors, rawSelfExtraction ) )

    val manufacturing: ValidatedNel[String, Vector[Recipe]] =
      rawManufacturing
        .traverseFilter( validateManufacturingRecipe( data, _ ) )
        .map( _ ++ nuclearWastePseudoRecipes( data.items, data ) )

    val defaultResourceOptions: ValidatedNel[String, ResourceOptions] =
      ResourceOptions.init( data.items, mapConfig ).toValidatedNel

    ( extractionRecipes, manufacturing, defaultResourceOptions )
      .mapN(
        ( ex, mf, ro ) =>
          Model(
            mf,
            data.items.to( SortedMap ),
            ex.map( _._1 ).distinct,
            ex,
            ro
          )
      )
  }

  def isSelfExtraction[M, N]( recipe: GameRecipe ): Boolean =
    recipe.ingredients == List( recipe.products.head )

  def validateItem( data: GameData, ccn: Countable[Double, ClassName] ): ValidatedNel[String, Countable[Double, Item]] =
    data.items
      .get( ccn.item )
      .toValidNel( show"Unknown item class ${ccn.item}" )
      .map( it => Countable( it, ccn.amount / it.form.simpleAmountFactor ) )

  def validateRecipeItems[F[_]: Traverse](
      data: GameData,
      items: F[Countable[Double, ClassName]]
  ): ValidatedNel[String, F[Countable[Double, Item]]] =
    items.traverse( validateItem( data, _ ) )

  def getExtractionRecipes(
      data: GameData,
      machines: Map[ClassName, ( Extractor, Machine )],
      selfExtraction: Vector[GameRecipe]
  ): ValidatedNel[String, Vector[( Item, ResourcePurity, Recipe )]] = {
    val ( miners, otherExtractors ) =
      machines.values.toVector.partition( _._2.machineType.is( ExtractorType.Miner ) )

    getConverterRecipes( data, miners, selfExtraction ).map(
      converterRecipes =>
        (converterRecipes ++ getOtherExtractionRecipes( data, otherExtractors ))
          .flatMap {
            case ( item, extractor, machine ) =>
              ResourcePurity.values
                .map( purity => ( item, purity, extractionRecipe( item, extractor, purity, machine ) ) )
          }
    )
  }

  def getConverterRecipes(
      data: GameData,
      converterExtractors: Vector[( Extractor, Machine )],
      selfExtraction: Vector[GameRecipe]
  ): ValidatedNel[String, Vector[( Item, Extractor, Machine )]] = {
    selfExtraction
      .traverse( r => validateRecipeItems[Id]( data, r.products.head ) )
      .map(
        selfExtractionItems =>
          converterExtractors.flatMap {
            case ( extractor, machine ) => selfExtractionItems.map( item => ( item.item, extractor, machine ) )
          }
      )
  }

  def getOtherExtractionRecipes(
      data: GameData,
      extractors: Vector[( Extractor, Machine )]
  ): Vector[( Item, Extractor, Machine )] =
    for {
      ( extractor, machine ) <- extractors
      allowedResources       <- extractor.allowedResources.toVector
      resource               <- allowedResources.toList.toVector
      item                   <- data.items.get( resource )
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
  ): Recipe =
    Recipe(
      ClassName( show"${item.className}_${purity.entryName.capitalize}_${extractor.className}" ),
      show"${item.displayName} ($purity, ${extractor.displayName})",
      Nil,
      NonEmptyList.of(
        Countable( item, extractor.itemsPerCycle.toDouble / item.form.simpleAmountFactor * purity.multiplier )
      ),
      extractor.cycleTime,
      machine,
      Power.Fixed( extractor.powerConsumption )
    )

  def validateManufacturer( data: GameData, className: ClassName ): ValidatedNel[String, Machine] =
    data.manufacturers
      .get( className )
      .map( Machine.manufacturer )
      .toValidNel( show"Unknown machine class $className" )

  def recipePower( recipe: GameRecipe, manufacturer: Machine ): Power =
    if (manufacturer.machineType.is( ManufacturerType.VariableManufacturer ))
      Power.Variable( recipe.variablePowerMin, recipe.variablePowerMin + recipe.variablePowerRange )
    else
      Power.Fixed( manufacturer.powerConsumption )

  def validateManufacturingRecipe(
      data: GameData,
      recipe: GameRecipe
  ): ValidatedNel[String, Option[Recipe]] =
    NonEmptyList
      .fromList( recipe.producedIn.filter( data.manufacturers.keySet ) )
      .traverse(
        ms =>
          (
            Option
              .when( ms.size == 1 )( ms.head )
              .toValidNel( show"Recipe ${recipe.displayName} is produced in multiple manufacturers" )
              .andThen( validateManufacturer( data, _ ) ),
            validateRecipeItems( data, recipe.ingredients ),
            validateRecipeItems( data, recipe.products )
          ).mapN(
            ( producer, ingredients, products ) =>
              Recipe(
                recipe.className,
                recipe.displayName,
                ingredients,
                products,
                recipe.duration,
                producer,
                recipePower( recipe, producer )
              )
          )
      )

  def nuclearWastePseudoRecipes( items: Map[ClassName, Item], gameData: GameData ): Vector[Recipe] =
    gameData.nuclearGenerators.values.flatMap { g =>
      g.fuels.flatMap {
        case ( src, prod ) =>
          ( items.get( src ), items.get( prod.item ) ).mapN(
            ( s, p ) =>
              Recipe(
                ClassName( s"${g.className.name}__${s.className.name}" ),
                p.displayName,
                Countable( s, 1d ) :: Nil,
                NonEmptyList.of( Countable( p, prod.amount.toDouble ) ),
                math.ceil( 1000 * s.fuelValue / g.powerProduction ).toLong.milliseconds,
                Machine( g.className, g.displayName, MachineType( ManufacturerType.Manufacturer ), 0d ),
                Power.Fixed( -g.powerProduction )
              )
          )
      }
    }.toVector

  implicit val modelShow: Show[Model] = Show.show { model =>
    implicit val showItem: Show[Item]       = Show.show( _.displayName )
    implicit val showMachine: Show[Machine] = Show.show( _.displayName )

    show"""Manufacturing Recipes
          |${model.manufacturingRecipes.map( _.show ).intercalate( "\n" )}
          |
          |Items
          |${model.items.values.map( _.toString ).intercalate( "\n" )}
          |
          |Extracted Items ${model.extractedItems.map( _.displayName ).intercalate( ", " )}
          |
          |Extraction Recipes
          |${model.extractionRecipes.map( _._3 ).map( _.show ).intercalate( "\n" )}
          |
          |Resource nodes
          |${model.defaultResourceOptions.show.linesIterator.map( "  " + _ ).toSeq.mkString_( "\n" )}
          |""".stripMargin
  }

}
