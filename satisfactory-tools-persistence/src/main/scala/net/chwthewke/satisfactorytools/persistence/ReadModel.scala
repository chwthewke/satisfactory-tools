package net.chwthewke.satisfactorytools
package persistence

import cats.Order.catsKernelOrderingForOrder
import cats.data.NonEmptyList
import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.traverse._
import doobie._
import doobie.implicits._
import scala.collection.immutable.SortedMap
import scala.concurrent.duration.FiniteDuration

import data.ClassName
import data.Countable
import data.Item
import model.ExtractorType
import model.Machine
import model.Model
import model.Power
import model.Recipe
import model.ResourceDistrib
import model.ResourceOptions
import model.ResourcePurity
import model.ResourceWeights

object ReadModel {

  private implicit val powerRead: Read[Power] =
    Read[( Double, Option[Double] )].map {
      case ( value, None )      => Power.Fixed( value )
      case ( min, Some( max ) ) => Power.Variable( min, max )
    }

  private[persistence] object statements {

    def selectItems( version: Int ): Query0[( ItemId, Item )] =
      // language=SQL
      sql"""SELECT 
           |    "id"
           |  , "class_name"
           |  , "display_name"
           |  , "form"
           |  , "energy_value"
           |  , "sink_points"
           |FROM "items"
           |WHERE "data_version" = $version
           |""".stripMargin //
      .query

    def selectMachines( version: Int ): Query0[( MachineId, Machine )] =
      // language=SQL
      sql"""SELECT
           |    "id"
           |  , "class_name"
           |  , "display_name"
           |  , "machine_type"
           |  , "power_consumption"
           |FROM "machines"
           |WHERE "data_version" = $version
           |""".stripMargin //
      .query

    type RecipeRow = ( RecipeId, ( ClassName, String, FiniteDuration, MachineId, Power ), Countable[Double, ItemId] )

    def selectRecipes( version: Int ): Query0[RecipeRow] =
      // language=SQL
      sql"""SELECT
           |    r."id"
           |  , r."class_name"
           |  , r."display_name"
           |  , r."duration_ms"
           |  , r."produced_in"
           |  , r."power"
           |  , r."power_var"
           |  , p."item_id"
           |  , p."amount"
           |FROM         "recipes"          r
           |  INNER JOIN "recipe_products"  p  ON r."id" = p."recipe_id"
           |WHERE r."data_version" = $version
           |ORDER BY r."id", p."id"
           |""".stripMargin //
      .query

    def selectRecipeIngredients( version: Int ): Query0[( RecipeId, Countable[Double, ItemId] )] =
      // language=SQL
      sql"""SELECT
           |    i."recipe_id"
           |  , i."item_id"
           |  , i."amount"
           |FROM          "recipe_ingredients"  i
           |  INNER JOIN  "recipes"             r  ON i."recipe_id" = r."id"
           |WHERE  r."data_version" = $version
           |ORDER BY i."recipe_id", i."id"
           |""".stripMargin //
      .query

    def selectExtractionRecipes( version: Int ): Query0[( ItemId, ResourcePurity, RecipeId )] =
      // language=SQL
      sql"""SELECT
           |    "item_id"
           |  , "purity"
           |  , "recipe_id"
           |FROM "extraction_recipes" x
           |INNER JOIN "recipes" r on x."recipe_id" = r."id"
           |WHERE r."data_version" = $version
           |ORDER BY "item_id"
           |""".stripMargin //
      .query

    def selectResourceNodes( version: Int ): Query0[( ExtractorType, ( ItemId, ResourceDistrib ) )] =
      // language=SQL
      sql"""SELECT
           |    "extractor_type"
           |  , "item_id"
           |  , "impure"
           |  , "normal"
           |  , "pure"
           |FROM "resource_nodes"
           |WHERE "data_version" = $version
           |ORDER BY "extractor_type"
           |""".stripMargin //
      .query
  }

  import statements._

  def readRecipes(
      itemsById: Map[ItemId, Item],
      machinesById: Map[MachineId, Machine],
      version: Int
  ): ConnectionIO[Vector[( RecipeId, Recipe )]] =
    for {
      rows <- Streams.groupAdjacentRows( selectRecipes( version ).stream ).compile.toVector
      ingredientRows <- selectRecipeIngredients( version ).stream
                         .map { case ( recipeId, ing ) => Map( recipeId -> Vector( ing ) ) }
                         .compile
                         .foldMonoid
    } yield for {
      (
        recipeId,
        ( className, displayName, duration, machineId, power ),
        productIds
      )          <- rows
      products   <- productIds.traverse( _.traverse( itemsById.get ) )
      producedIn <- machinesById.get( machineId )
      ingredients <- ingredientRows
                      .getOrElse( recipeId, Vector.empty )
                      .traverse( _.traverse( itemsById.get ) )
    } yield recipeId ->
      Recipe(
        className,
        displayName,
        ingredients.toList,
        NonEmptyList( products.head, products.tail.toList ),
        duration,
        producedIn,
        power
      )

  def readExtractionRecipes(
      itemsById: Map[ItemId, Item],
      recipesById: Map[RecipeId, Recipe],
      version: Int
  ): ConnectionIO[Vector[( Item, ResourcePurity, Recipe )]] = {

    selectExtractionRecipes( version ).stream
      .map {
        case ( itemId, purity, recipeId ) =>
          ( itemsById.get( itemId ), recipesById.get( recipeId ) ).mapN( ( _, purity, _ ) )
      }
      .unNone
      .compile
      .toVector
  }

  def readResourceNodes(
      itemsById: Map[ItemId, Item],
      version: Int
  ): ConnectionIO[Map[ExtractorType, Map[Item, ResourceDistrib]]] =
    Streams
      .groupAdjacentByFirstNev( selectResourceNodes( version ).stream )
      .map {
        case ( extractorType, items ) =>
          (
            extractorType,
            items.toVector.flatMap { case ( itemId, distrib ) => itemsById.get( itemId ).tupleRight( distrib ) }.toMap
          )
      }
      .compile
      .to( Map )

  def readModel( version: Int ): ConnectionIO[Model] =
    for {
      itemsById         <- selectItems( version ).toMap
      machinesById      <- selectMachines( version ).toMap
      recipesById       <- readRecipes( itemsById, machinesById, version ).map( _.to( SortedMap ) )
      extractionRecipes <- readExtractionRecipes( itemsById, recipesById, version )
      resourceNodes     <- readResourceNodes( itemsById, version )
    } yield {

      val extractionRecipeClasses: Set[ClassName] =
        extractionRecipes.foldMap { case ( _, _, recipe ) => Set( recipe.className ) }

      val manufacturingRecipes = recipesById.values.filterNot( r => extractionRecipeClasses( r.className ) ).toVector

      val extractedItems =
        extractionRecipes.map( _._1 ).distinctBy( _.className )

      Model(
        manufacturingRecipes,
        itemsById.values.map( it => ( it.className, it ) ).to( SortedMap ),
        extractedItems,
        extractionRecipes,
        ResourceOptions( resourceNodes, ResourceWeights.default )
      )
    }

}
