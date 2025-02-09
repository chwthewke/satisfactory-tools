package net.chwthewke.satisfactorytools
package loader

import alleycats.std.map._
import cats.Id
import cats.Traverse
import cats.data.NonEmptyList
import cats.data.ValidatedNel
import cats.syntax.all._
import scala.collection.immutable.SortedMap
import scala.concurrent.duration._

import data.ClassName
import data.Countable
import data.Extractor
import data.Form
import data.GameData
import data.GameRecipe
import data.Item
import data.Manufacturer
import data.NativeClass
import model.ExtractorType
import model.Machine
import model.MachineType
import model.ManufacturerType
import model.Model
import model.ModelVersion
import model.Power
import model.Recipe
import model.RecipeCategory
import model.ResourceOptions
import model.ResourcePurity
import model.ResourceWeights

private[loader] object ModelInit {
  def apply( version: ModelVersion, data: GameData, mapConfig: MapConfig ): ValidatedNel[String, Model] = {
    val ( rawSelfExtraction, rawManufacturing ) = data.recipes.partition( _.isSelfExtraction )

    val extractorMachines: ValidatedNel[String, Map[ClassName, ( Extractor, Machine )]] =
      data.extractors.traverse( ex => extractorMachine( ex ).toValidatedNel.tupleLeft( ex ) )

    val extractionRecipes: ValidatedNel[String, Vector[( Item, ResourcePurity, Recipe )]] =
      extractorMachines.andThen( extractors => getExtractionRecipes( data, extractors, rawSelfExtraction ) )

    val manufacturingRecipeClassification: Map[ClassName, RecipeCategory] = RecipeClassifier( data ).classifyRecipes

    val manufacturing: ValidatedNel[String, Vector[Recipe]] =
      rawManufacturing
        .traverseFilter( validateManufacturingRecipe( data, manufacturingRecipeClassification, _ ) )
        .map( _ ++ nuclearWastePseudoRecipes( data.items, data ) )

    val defaultResourceOptions: ValidatedNel[String, ResourceOptions] =
      initResourceOptions( data.items, mapConfig ).toValidatedNel

    ( extractionRecipes, manufacturing, defaultResourceOptions )
      .mapN( ( ex, mf, ro ) =>
        Model(
          version,
          mf,
          data.items.map { case ( cn, ( item, _ ) ) => ( cn, item ) }.to( SortedMap ),
          ex.map( _._1 ).distinct,
          ex,
          ro
        )
      )
  }

  def initResourceOptions(
      modelItems: Map[ClassName, ( Item, NativeClass )],
      config: MapConfig
  ): Either[String, ResourceOptions] =
    config.resourceNodes
      .traverse(
        _.toVector
          .traverse {
            case ( itemClass, distrib ) =>
              modelItems.get( itemClass ).toValidNel( itemClass ).as( itemClass ).tupleRight( distrib )
          }
          .map( _.toMap )
      )
      .map( ResourceOptions( _, ResourceWeights.default ) )
      .leftMap( _.mkString_( "Unknown items in resource nodes config: ", ", ", "" ) )
      .toEither

  def validateItem(
      data: GameData,
      ccn: Countable[Double, ClassName]
  ): ValidatedNel[String, Countable[Double, Item]] =
    data.items
      .get( ccn.item )
      .map( _._1 )
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

    getConverterRecipes( data, miners, selfExtraction ).map( converterRecipes =>
      ( converterRecipes ++ getOtherExtractionRecipes( data, otherExtractors ) )
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
      .traverse( r => validateRecipeItems[Id]( data, r.products.head ).map( _.item ) )
      .map { selfExtractionItems =>
        val ores: Vector[Item] =
          data.items.values.collect {
            case ( item, NativeClass.resourceDescClass ) if item.form == Form.Solid => item
          }.toVector
        ( ores ++ selfExtractionItems.filter( _.form == Form.Solid ) ).distinct
      }
      .map( ores =>
        converterExtractors.flatMap {
          case ( extractor, machine ) => ores.map( item => ( item, extractor, machine ) )
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
      ( item, _ )            <- data.items.get( resource )
    } yield ( item, extractor, machine )

  def extractorMachine( extractor: Extractor ): Either[String, Machine] =
    ExtractorType.values
      .find( _.dataKey.fold( _ == extractor.extractorTypeName, _ == extractor.className ) )
      .toRight( s"No known extractor type for class ${extractor.className}, type ${extractor.extractorTypeName}" )
      .map( exType =>
        Machine(
          extractor.className,
          extractor.displayName,
          MachineType( exType ),
          extractor.powerConsumption,
          extractor.powerConsumptionExponent
        )
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
      RecipeCategory.Extraction,
      Nil,
      NonEmptyList.of(
        Countable( item, extractor.itemsPerCycle.toDouble / item.form.simpleAmountFactor * purity.multiplier )
      ),
      extractor.cycleTime,
      machine,
      Power.Fixed( extractor.powerConsumption )
    )

  def manufacturerMachine( manufacturer: Manufacturer ): Machine =
    Machine(
      manufacturer.className,
      manufacturer.displayName,
      MachineType(
        if (manufacturer.isCollider) ManufacturerType.VariableManufacturer
        else ManufacturerType.Manufacturer
      ),
      manufacturer.powerConsumption,
      manufacturer.powerConsumptionExponent
    )

  def validateManufacturer( data: GameData, className: ClassName ): ValidatedNel[String, Machine] =
    data.manufacturers
      .get( className )
      .map( manufacturerMachine )
      .toValidNel( show"Unknown machine class $className" )

  def recipePower( recipe: GameRecipe, manufacturer: Machine ): Power =
    if (manufacturer.machineType.is( ManufacturerType.VariableManufacturer ))
      Power.Variable( recipe.variablePowerMin, recipe.variablePowerMin + recipe.variablePowerRange )
    else
      Power.Fixed( manufacturer.powerConsumption )

  def validateManufacturingRecipe(
      data: GameData,
      classification: Map[ClassName, RecipeCategory],
      recipe: GameRecipe
  ): ValidatedNel[String, Option[Recipe]] =
    NonEmptyList
      .fromList( recipe.producedIn.filter( data.manufacturers.keySet ) )
      .traverse( ms =>
        (
          classification
            .get( recipe.className )
            .toValidNel( show"Recipe ${recipe.displayName} [${recipe.className}] not classified" ),
          Option
            .when( ms.size == 1 )( ms.head )
            .toValidNel(
              show"Recipe ${recipe.displayName} [${recipe.className}] is produced in multiple manufacturers"
            )
            .andThen( validateManufacturer( data, _ ) ),
          validateRecipeItems( data, recipe.ingredients ),
          validateRecipeItems( data, recipe.products )
        ).mapN( ( cat, producer, ingredients, products ) =>
          Recipe(
            recipe.className,
            recipe.displayName,
            cat,
            ingredients,
            products,
            recipe.duration,
            producer,
            recipePower( recipe, producer )
          )
        )
      )

  def nuclearWastePseudoRecipes( items: Map[ClassName, ( Item, NativeClass )], gameData: GameData ): Vector[Recipe] =
    gameData.nuclearGenerators.values.flatMap { g =>
      g.fuels.flatMap {
        case ( src, prod ) =>
          ( items.get( src ), items.get( prod.item ) ).mapN {
            case ( ( s, _ ), ( p, _ ) ) =>
              Recipe(
                ClassName( s"${g.className.name}__${s.className.name}" ),
                p.displayName,
                RecipeCategory.NuclearWaste,
                Countable( s, 1d ) :: Nil,
                NonEmptyList.of( Countable( p, prod.amount.toDouble ) ),
                math.ceil( 1000 * s.fuelValue / g.powerProduction ).toLong.milliseconds,
                Machine(
                  g.className,
                  g.displayName,
                  MachineType( ManufacturerType.Manufacturer ),
                  0d,
                  g.powerConsumptionExponent
                ),
                Power.Fixed( -g.powerProduction )
              )
          }
      }
    }.toVector

}
