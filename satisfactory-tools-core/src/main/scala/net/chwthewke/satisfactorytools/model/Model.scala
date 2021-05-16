package net.chwthewke.satisfactorytools
package model

import alleycats.std.iterable._
import alleycats.std.map._
import cats.Id
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
import mouse.option._
import scala.collection.immutable.SortedMap

import data.ClassName
import data.Countable
import data.Extractor
import data.GameData
import data.Item

case class Model(
    manufacturingRecipes: Vector[Recipe[Machine, Item]],
    items: SortedMap[ClassName, Item],
    extractedItems: Vector[Item],
    extractionRecipes: Vector[( Item, ResourcePurity, Recipe[Machine, Item] )],
    defaultResourceOptions: ResourceOptions
)

object Model {

  def init( data: GameData, mapConfig: MapConfig ): ValidatedNel[String, Model] = {
    val ( rawSelfExtraction, rawManufacturing ) = data.recipes.partition( isSelfExtraction )

    val extractorMachines = data.extractors.traverse( ex => Machine.extractor( ex ).toValidatedNel.tupleLeft( ex ) )

    val extractionRecipes: ValidatedNel[String, Vector[( Item, ResourcePurity, Recipe[Machine, Item] )]] =
      ( extractorMachines, rawSelfExtraction.traverse( validateRecipeItems( data, _ ) ) )
        .mapN( getExtractionRecipes( data, _, _ ) )

    val manufacturing: ValidatedNel[String, Vector[Recipe[Machine, Item]]] =
      rawManufacturing.traverseFilter( validateManufacturingRecipe( data, _ ) )

    val defaultResourceOptions: ValidatedNel[String, ResourceOptions] =
      ResourceOptions.init( data.items, mapConfig ).toValidatedNel

    ( extractionRecipes, manufacturing, defaultResourceOptions )
      .mapN( ( ex, mf, ro ) => Model( mf, data.items.to( SortedMap ), ex.map( _._1 ).distinct, ex, ro ) )
  }

  def isSelfExtraction[M, N]( recipe: Recipe[M, N] ): Boolean =
    recipe.ingredients == List( recipe.products.head )

  def validateItem( data: GameData, ccn: Countable[Double, ClassName] ): ValidatedNel[String, Countable[Double, Item]] =
    data.items
      .get( ccn.item )
      .toValidNel( show"Unknown item class ${ccn.item}" )
      .map( it => Countable( it, ccn.amount / it.form.simpleAmountFactor ) )

  def validateRecipeItems[A]( data: GameData, recipe: Recipe[A, ClassName] ): ValidatedNel[String, Recipe[A, Item]] =
    recipe
      .traverseIngredientsAndProducts( validateItem( data, _ ) )
      .leftMap( errs => NonEmptyList.of( show"In ${recipe.displayName}\n  ${errs.intercalate( "\n  " )}" ) )

  def getExtractionRecipes(
      data: GameData,
      machines: Map[ClassName, ( Extractor, Machine )],
      selfExtraction: Vector[Recipe[List[ClassName], Item]]
  ): Vector[( Item, ResourcePurity, Recipe[Machine, Item] )] = {
    val ( miners, otherExtractors ) =
      machines.values.toVector.partition( _._2.machineType == MachineType.Extractor( ExtractorType.Miner ) )

    (
      getConverterRecipes( miners, selfExtraction ) ++
        getOtherExtractionRecipes( data, otherExtractors )
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

  def validateManufacturer( data: GameData, className: ClassName ): ValidatedNel[String, Machine] =
    data.manufacturers
      .get( className )
      .map( Machine.manufacturer )
      .toValidNel( show"Unknown machine class $className" )

  def validateManufacturingRecipe(
      data: GameData,
      recipe: Recipe[List[ClassName], ClassName]
  ): ValidatedNel[String, Option[Recipe[Machine, Item]]] =
    NonEmptyList
      .fromList( recipe.producedIn.filter( data.manufacturers.keySet ) )
      .traverse(
        ms =>
          (
            Option
              .when( ms.size == 1 )( ms.head )
              .toValidNel( show"Recipe ${recipe.displayName} is produced in multiple manufacturers" )
              .andThen( validateManufacturer( data, _ ) ),
            validateRecipeItems( data, recipe )
          ).mapN( ( producer, itemRecipe ) => itemRecipe.copy( producedIn = producer ) )
      )

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
          |${model.extractionRecipes.map( _._2 ).map( _.show ).intercalate( "\n" )}
          |
          |Resource nodes
          |${model.defaultResourceOptions.show.linesIterator.map( "  " + _ ).toSeq.mkString_( "\n" )}
          |""".stripMargin
  }

}
