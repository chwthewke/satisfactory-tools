package net.chwthewke.satisfactorytools
package persistence

import cats.data.NonEmptyList
import cats.data.OptionT
import cats.syntax.all._
import doobie._
import doobie.implicits._
import doobie.util.invariant.UnexpectedContinuation
import doobie.util.invariant.UnexpectedEnd
import scala.collection.immutable.SortedMap
import scala.concurrent.duration.FiniteDuration

import api.ModelApi
import data.ClassName
import data.Countable
import data.Item
import model.ExtractorType
import model.Machine
import model.Model
import model.ModelVersion
import model.Power
import model.Recipe
import model.RecipeCategory
import model.ResourceDistrib
import model.ResourceOptions
import model.ResourcePurity
import model.ResourceWeights
import protocol.ModelVersionId

object ReadModel extends ModelApi[ConnectionIO] {

  private implicit val powerRead: Read[Power] =
    Read[( Double, Option[Double] )].map {
      case ( value, None )      => Power.Fixed( value )
      case ( min, Some( max ) ) => Power.Variable( min, max )
    }

  private[persistence] object statements {

    def selectItems( version: ModelVersionId ): Query0[( ItemId, Item )] =
      // language=SQL
      sql"""SELECT 
           |    "id"
           |  , "class_name"
           |  , "display_name"
           |  , "form"
           |  , "energy_value"
           |  , "sink_points"
           |  , "small_icon_package_dir"
           |  , "small_icon_package_name"
           |  , "small_icon_texture_name"
           |FROM "items"
           |WHERE "model_version_id" = $version
           |""".stripMargin //
        .query

    def selectMachines( version: ModelVersionId ): Query0[( MachineId, Machine )] =
      // language=SQL
      sql"""SELECT
           |    "id"
           |  , "class_name"
           |  , "display_name"
           |  , "machine_type"
           |  , "power_consumption"
           |  , "power_consumption_exponent"
           |FROM "machines"
           |WHERE "model_version_id" = $version
           |""".stripMargin //
        .query

    type RecipeRow = (
        RecipeId,
        ( ClassName, String, Option[Int], Option[String], FiniteDuration, MachineId, Power ),
        Countable[Double, ItemId]
    )

    def selectRecipes( version: ModelVersionId ): Query0[RecipeRow] =
      // language=SQL
      sql"""SELECT
           |    r."id"
           |  , r."class_name"
           |  , r."display_name"
           |  , r."tier"
           |  , r."category"
           |  , r."duration_ms"
           |  , r."produced_in"
           |  , r."power"
           |  , r."power_var"
           |  , p."item_id"
           |  , p."amount"
           |FROM         "recipes"          r
           |  INNER JOIN "recipe_products"  p  ON r."id" = p."recipe_id"
           |WHERE r."model_version_id" = $version
           |ORDER BY r."id", p."id"
           |""".stripMargin //
        .query

    def selectRecipeIngredients( version: ModelVersionId ): Query0[( RecipeId, Countable[Double, ItemId] )] =
      // language=SQL
      sql"""SELECT
           |    i."recipe_id"
           |  , i."item_id"
           |  , i."amount"
           |FROM          "recipe_ingredients"  i
           |  INNER JOIN  "recipes"             r  ON i."recipe_id" = r."id"
           |WHERE  r."model_version_id" = $version
           |ORDER BY i."recipe_id", i."id"
           |""".stripMargin //
        .query

    def selectExtractionRecipes( version: ModelVersionId ): Query0[( ItemId, ResourcePurity, RecipeId )] =
      // language=SQL
      sql"""SELECT
           |    "item_id"
           |  , "purity"
           |  , "recipe_id"
           |FROM "extraction_recipes" x
           |INNER JOIN "recipes" r on x."recipe_id" = r."id"
           |WHERE r."model_version_id" = $version
           |ORDER BY "item_id"
           |""".stripMargin //
        .query

    def selectResourceNodes( version: ModelVersionId ): Query0[( ExtractorType, ( ItemId, ResourceDistrib ) )] =
      // language=SQL
      sql"""SELECT
           |    "extractor_type"
           |  , "item_id"
           |  , "impure"
           |  , "normal"
           |  , "pure"
           |FROM "resource_nodes"
           |WHERE "model_version_id" = $version
           |ORDER BY "extractor_type"
           |""".stripMargin //
        .query

    def selectItemIds( version: ModelVersionId ): Query0[( ClassName, ItemId )] =
      // language=SQL
      sql"""SELECT "class_name", "id"
           |FROM "items"
           |WHERE "model_version_id" = $version
           |""".stripMargin //
        .query

    def selectRecipeIds( version: ModelVersionId ): Query0[( ClassName, RecipeId )] =
      // language=SQL
      sql"""SELECT "class_name", "id"
           |FROM "recipes"
           |WHERE "model_version_id" = $version
           |""".stripMargin //
        .query

    def selectModelVersion( version: ModelVersionId ): Query0[ModelVersion] =
      // language=SQL
      sql"""SELECT "version", "description"
           |FROM "model_versions"
           |WHERE "family" = 'SATISFACTORY' :: T_MODEL_FAMILY
           |  AND "id" = $version 
           |""".stripMargin //
        .query

    val selectModelVersions: Query0[( ModelVersionId, ModelVersion )] =
      // language=SQL
      sql"""SELECT "id", "version", "description"
           |FROM "model_versions"
           |WHERE "family" = 'SATISFACTORY' :: T_MODEL_FAMILY
           |ORDER BY "version"
           |""".stripMargin //
        .query
  }

  import statements._

  private def buildRecipes(
      itemsById: Map[ItemId, Item],
      machinesById: Map[MachineId, Machine],
      version: ModelVersionId
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
        ( className, displayName, tierOpt, catOpt, duration, machineId, power ),
        productIds
      )          <- rows
      category   <- RecipeCategory.of( tierOpt, catOpt ).toOption
      products   <- productIds.traverse( _.traverse( itemsById.get ) )
      producedIn <- machinesById.get( machineId )
      ingredients <- ingredientRows
                       .getOrElse( recipeId, Vector.empty )
                       .traverse( _.traverse( itemsById.get ) )
    } yield recipeId ->
      Recipe(
        className,
        displayName,
        category,
        ingredients.toList,
        NonEmptyList( products.head, products.tail.toList ),
        duration,
        producedIn,
        power
      )

  private def readExtractionRecipes(
      itemsById: Map[ItemId, Item],
      recipesById: Map[RecipeId, Recipe],
      version: ModelVersionId
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

  private def readResourceNodes(
      itemsById: Map[ItemId, Item],
      version: ModelVersionId
  ): ConnectionIO[Map[ExtractorType, Map[ClassName, ResourceDistrib]]] =
    Streams
      .groupAdjacentByFirstNev( selectResourceNodes( version ).stream )
      .map {
        case ( extractorType, items ) =>
          (
            extractorType,
            items.toVector.flatMap {
              case ( itemId, distrib ) => itemsById.get( itemId ).map( item => ( item.className, distrib ) )
            }.toMap
          )
      }
      .compile
      .to( Map )

  def readModel( version: ModelVersionId ): ConnectionIO[Model] =
    for {
      modelVersion <- selectModelVersion( version ).unique
                        .adaptErr {
                          case UnexpectedEnd          => Error( s"No model version $version" )
                          case UnexpectedContinuation => Error( s"Multiple model versions $version" )
                        }
      itemsById         <- selectItems( version ).toMap
      machinesById      <- selectMachines( version ).toMap
      recipesById       <- buildRecipes( itemsById, machinesById, version ).map( _.to( SortedMap ) )
      extractionRecipes <- readExtractionRecipes( itemsById, recipesById, version )
      resourceNodes     <- readResourceNodes( itemsById, version )
    } yield {

      val extractionRecipeClasses: Set[ClassName] =
        extractionRecipes.foldMap { case ( _, _, recipe ) => Set( recipe.className ) }

      val manufacturingRecipes = recipesById.values.filterNot( r => extractionRecipeClasses( r.className ) ).toVector

      val extractedItems =
        extractionRecipes.map( _._1 ).distinctBy( _.className )

      Model(
        modelVersion,
        manufacturingRecipes,
        itemsById.values.map( it => ( it.className, it ) ).to( SortedMap ),
        extractedItems,
        extractionRecipes,
        ResourceOptions( resourceNodes, ResourceWeights.default )
      )
    }

  override def getModelVersions: ConnectionIO[Vector[( ModelVersionId, ModelVersion )]] =
    selectModelVersions.to[Vector]

  override def getModel( version: ModelVersionId ): OptionT[ConnectionIO, Model] =
    OptionT
      .liftF( readModel( version ) )
      .filter( model =>
        model.manufacturingRecipes.nonEmpty &&
          model.items.nonEmpty &&
          model.extractedItems.nonEmpty &&
          model.extractionRecipes.nonEmpty
      )

  def readItemIds( version: ModelVersionId ): ConnectionIO[Map[ClassName, ItemId]] =
    selectItemIds( version ).toMap

  def readItems( version: ModelVersionId ): ConnectionIO[Map[ItemId, Item]] =
    selectItems( version ).toMap

  def readRecipeIds( version: ModelVersionId ): ConnectionIO[Map[ClassName, RecipeId]] =
    selectRecipeIds( version ).toMap

  def readRecipes( version: ModelVersionId ): ConnectionIO[Map[RecipeId, Recipe]] =
    for {
      itemsById    <- readItems( version )
      machinesById <- selectMachines( version ).toMap
      recipes      <- buildRecipes( itemsById, machinesById, version )
    } yield recipes.toMap

  def readDefaultResourceOptions( version: ModelVersionId ): ConnectionIO[ResourceOptions] =
    for {
      itemsById     <- readItems( version )
      resourceNodes <- readResourceNodes( itemsById, version )
    } yield ResourceOptions( resourceNodes, ResourceWeights.default )

}
