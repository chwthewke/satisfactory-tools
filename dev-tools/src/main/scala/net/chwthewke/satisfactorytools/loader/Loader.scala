package net.chwthewke.satisfactorytools
package loader

import alleycats.std.map._
import cats.Id
import cats.Monoid
import cats.Order.catsKernelOrderingForOrder
import cats.Traverse
import cats.data.NonEmptyList
import cats.data.ValidatedNel
import cats.effect.IO
import cats.effect.Sync
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.show._
import cats.syntax.traverse._
import cats.syntax.traverseFilter._
import cats.syntax.validated._
import fs2.Stream
import io.circe.Decoder
import io.circe.fs2.byteArrayParser
import io.circe.fs2.decoder
import java.io.InputStream
import pureconfig.ConfigSource
import pureconfig.module.catseffect.syntax._
import scala.collection.immutable.SortedMap
import scala.concurrent.duration._

import data.ClassName
import data.Countable
import data.Extractor
import data.GameData
import data.GameRecipe
import data.Item
import data.Manufacturer
import model.ExtractorType
import model.Machine
import model.MachineType
import model.ManufacturerType
import model.Model
import model.ModelVersion
import model.Power
import model.Recipe
import model.ResourceOptions
import model.ResourcePurity
import model.ResourceWeights

class Loader[F[_]]( implicit val syncInstance: Sync[F] ) {

  def docsResource( storage: DataVersionStorage ): F[InputStream] =
    syncInstance.delay( getClass.getClassLoader.getResourceAsStream( s"${storage.docsKey}/Docs.json" ) )

  def streamDocsResource( storage: DataVersionStorage ): Stream[F, Byte] =
    fs2.io.readInputStream( docsResource( storage ), 32768 )

  private def process[A: Decoder: Monoid]( bytes: Stream[F, Byte] ): F[A] =
    bytes
      .through( byteArrayParser[F] )
      .through( decoder[F, A] )
      .compile
      .foldMonoid

  def loadResource[A: Monoid: Decoder]( storage: DataVersionStorage ): F[A] =
    process[A]( streamDocsResource( storage ) )

  def loadGameData( storage: DataVersionStorage ): F[GameData] = loadResource[GameData]( storage )

  def loadModel( storage: DataVersionStorage ): F[Model] =
    (
      loadGameData( storage ),
      loadMapConfig( storage )
    ).tupled
      .flatMap {
        case ( data, map ) => Loader.ModelInit( storage.modelVersion, data, map ).leftMap( Error( _ ) ).liftTo[F]
      }

  def loadMapConfig( storage: DataVersionStorage ): F[MapConfig] =
    loadMapConfigs.flatMap(
      _.configs
        .get( storage.modelVersion.version )
        .liftTo[F]( new NoSuchElementException( s"No map config for version ${storage.modelVersion}" ) )
    )

  def loadMapConfigs: F[MapConfigSet] = Loader.mapConf.loadF[F, MapConfigSet]()

}

object Loader {

  private object ModelInit {
    def apply( version: ModelVersion, data: GameData, mapConfig: MapConfig ): ValidatedNel[String, Model] = {
      val ( rawSelfExtraction, rawManufacturing ) = data.recipes.partition( isSelfExtraction )

      val extractorMachines: ValidatedNel[String, Map[ClassName, ( Extractor, Machine )]] =
        data.extractors.traverse( ex => extractorMachine( ex ).toValidatedNel.tupleLeft( ex ) )

      val extractionRecipes: ValidatedNel[String, Vector[( Item, ResourcePurity, Recipe )]] =
        extractorMachines.andThen( extractors => getExtractionRecipes( data, extractors, rawSelfExtraction ) )

      val manufacturing: ValidatedNel[String, Vector[Recipe]] =
        rawManufacturing
          .traverseFilter( validateManufacturingRecipe( data, _ ) )
          .map( _ ++ nuclearWastePseudoRecipes( data.items, data ) )

      val defaultResourceOptions: ValidatedNel[String, ResourceOptions] =
        initResourceOptions( data.items, mapConfig ).toValidatedNel

      ( extractionRecipes, manufacturing, defaultResourceOptions )
        .mapN(
          ( ex, mf, ro ) =>
            Model(
              version,
              mf,
              data.items.to( SortedMap ),
              ex.map( _._1 ).distinct,
              ex,
              ro
            )
        )
    }

    def initResourceOptions( modelItems: Map[ClassName, Item], config: MapConfig ): Either[String, ResourceOptions] =
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

    def isSelfExtraction( recipe: GameRecipe ): Boolean =
      recipe.ingredients == List( recipe.products.head )

    def validateItem(
        data: GameData,
        ccn: Countable[Double, ClassName]
    ): ValidatedNel[String, Countable[Double, Item]] =
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

    def extractorMachine( extractor: Extractor ): Either[String, Machine] =
      ExtractorType.values
        .find( _.dataKey.fold( _ == extractor.extractorTypeName, _ == extractor.className ) )
        .toRight( s"No known extractor type for class ${extractor.className}, type ${extractor.extractorTypeName}" )
        .map(
          exType =>
            Machine(
              extractor.className,
              extractor.displayName,
              MachineType( exType ),
              extractor.powerConsumption
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
          if (manufacturer.powerConsumption == 0d) ManufacturerType.VariableManufacturer
          else ManufacturerType.Manufacturer
        ),
        manufacturer.powerConsumption
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

  }

  val mapConf: ConfigSource = ConfigSource.resources( "map.conf" ).withFallback( ConfigSource.empty )

  def apply[F[_]: Sync]: Loader[F] = new Loader[F]

  val io: Loader[IO] = Loader[IO]

}
