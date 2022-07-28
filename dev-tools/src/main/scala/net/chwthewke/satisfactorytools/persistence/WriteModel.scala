package net.chwthewke.satisfactorytools
package persistence

import cats.Reducible
import cats.syntax.alternative._
import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.list._
import cats.syntax.traverse._
import cats.syntax.vector._
import doobie._
import doobie.implicits._
import scala.concurrent.duration.FiniteDuration

import data.ClassName
import data.Countable
import data.Item
import model.ExtractorType
import model.Machine
import model.Model
import model.ModelVersion
import model.Power
import model.Recipe
import model.ResourceDistrib
import model.ResourcePurity
import protocol.ModelVersionId

object WriteModel {

  implicit val powerWrite: Write[Power] =
    Write[( Double, Option[Double] )].contramap {
      case Power.Fixed( value )       => ( value, None )
      case Power.Variable( min, max ) => ( min, Some( max ) )
    }

  private[persistence] case class RecipeIngredientId( id: Int ) extends AnyVal
  private[persistence] case class RecipeProductId( id: Int )    extends AnyVal

  private[persistence] object statements {

    /** Returns `f IN ((fs0-A, fs0-B), (fs1-A, fs1-B), ...)`. */
    private def notIn[F[_]: Reducible, A: util.Put, B: util.Put]( f: Fragment, fs: F[( A, B )] ): Fragment =
      fs.toList.map { case ( a, b ) => fr0"($a,$b)" }.foldSmash1( f ++ fr0"NOT IN (", fr",", fr")" )

    /** Returns `f IN ((fs0-A, fs0-B, fs0-C), (fs1-A, fs1-B, fs1-C), ...)`. */
    private def notIn[F[_]: Reducible, A: util.Put, B: util.Put, C: util.Put](
        f: Fragment,
        fs: F[( A, B, C )]
    ): Fragment =
      fs.toList.map { case ( a, b, c ) => fr0"($a,$b,$c)" }.foldSmash1( f ++ fr0"NOT IN (", fr",", fr")" )

    /** Returns `AND (f1) AND (f2) AND ... (fn)` for all defined fragments. */
    def andOpt( fs: Option[Fragment]* ): Fragment =
      fs.toList.unite.toNel.foldMap( fs => fr"AND" ++ Fragments.and( fs.toList: _* ) )

    def insertItem( version: ModelVersionId ): Update[Item] =
      Update[( Item, ModelVersionId )](
        // language=SQL
        """INSERT INTO "items"
          |( "class_name"
          |, "display_name"
          |, "form"
          |, "energy_value"
          |, "sink_points"
          |, "small_icon_package_dir"
          |, "small_icon_package_name"
          |, "small_icon_texture_name"
          |, "model_version_id"
          |)
          |VALUES ( ?, ?, ?, ?, ?, ?, ?, ?, ? )
          |ON CONFLICT ON CONSTRAINT "items_name_version_unique"
          |DO UPDATE SET
          |    "display_name"            = "excluded"."display_name"
          |  , "form"                    = "excluded"."form"
          |  , "energy_value"            = "excluded"."energy_value"
          |  , "sink_points"             = "excluded"."sink_points"
          |  , "small_icon_package_dir"  = "excluded"."small_icon_package_dir"
          |  , "small_icon_package_name" = "excluded"."small_icon_package_name"
          |  , "small_icon_texture_name" = "excluded"."small_icon_texture_name"
          |""".stripMargin
      ).contramap( ( _, version ) )

    def insertMachine( version: ModelVersionId ): Update[Machine] =
      Update[( Machine, ModelVersionId )](
        // language=SQL
        """INSERT INTO "machines"
          |( "class_name"
          |, "display_name"
          |, "machine_type"
          |, "power_consumption"
          |, "model_version_id"
          |)
          |VALUES ( ?, ?, ?, ?, ? )
          |ON CONFLICT ON CONSTRAINT "machines_name_version_unique"
          |DO UPDATE SET
          |    "display_name"      = "excluded"."display_name"
          |  , "machine_type"      = "excluded"."machine_type" 
          |  , "power_consumption" = "excluded"."power_consumption"
          |""".stripMargin
      ).contramap( ( _, version ) )

    def insertRecipe( version: ModelVersionId ): Update[( Recipe, MachineId )] =
      Update[( ClassName, String, FiniteDuration, MachineId, Power, ModelVersionId )](
        // language=SQL
        """INSERT INTO "recipes"
          |( "class_name"
          |, "display_name"
          |, "duration_ms"
          |, "produced_in"
          |, "power"
          |, "power_var"
          |, "model_version_id"
          |)
          |VALUES ( ?, ?, ?, ?, ?, ?, ? )
          |ON CONFLICT ON CONSTRAINT "recipes_name_version_unique"
          |DO UPDATE SET
          |    "display_name" = "excluded"."display_name"
          |  , "duration_ms"  = "excluded"."duration_ms"
          |  , "produced_in"  = "excluded"."produced_in"
          |  , "power"        = "excluded"."power"
          |  , "power_var"    = "excluded"."power_var"
          |""".stripMargin
      ).contramap {
        case ( recipe, machineId ) =>
          ( recipe.className, recipe.displayName, recipe.duration, machineId, recipe.power, version )
      }

    val insertRecipeIngredient: Update[( RecipeId, Countable[Double, ItemId] )] =
      Update(
        // language=SQL
        """INSERT INTO "recipe_ingredients"
          |( "recipe_id"
          |, "item_id"
          |, "amount"
          |)
          |VALUES ( ?, ?, ? )
          |ON CONFLICT ON CONSTRAINT "recipe_ingredients_recipe_item_unique"
          |DO UPDATE SET
          |    "amount" = "excluded"."amount"
          |""".stripMargin
      )

    val insertRecipeProduct: Update[( RecipeId, Countable[Double, ItemId] )] =
      Update(
        // language=SQL
        """INSERT INTO "recipe_products"
          |( "recipe_id"
          |, "item_id"
          |, "amount"
          |)
          |VALUES ( ?, ?, ? )
          |ON CONFLICT ON CONSTRAINT "recipe_products_recipe_item_unique"
          |DO UPDATE SET
          |    "amount" = "excluded"."amount"
          |""".stripMargin
      )

    val insertExtractionRecipe: Update[( ItemId, ResourcePurity, RecipeId )] =
      Update(
        // language=SQL
        """INSERT INTO "extraction_recipes"
          |( "item_id"
          |, "purity"
          |, "recipe_id"
          |)
          |VALUES ( ?, ?, ? )
          |ON CONFLICT ON CONSTRAINT "extraction_recipes_item_purity_recipe_unique"
          |DO NOTHING
          |""".stripMargin
      )

    def insertResourceNodes( modelVersionId: ModelVersionId ): Update[( ExtractorType, ItemId, ResourceDistrib )] =
      Update[( ExtractorType, ItemId, ResourceDistrib, ModelVersionId )](
        // language=SQL
        """INSERT INTO "resource_nodes"
          |( "extractor_type"
          |, "item_id"
          |, "impure"
          |, "normal"
          |, "pure"
          |, "model_version_id"
          |)
          |VALUES ( ?, ?, ?, ?, ?, ? )
          |ON CONFLICT ON CONSTRAINT "resource_nodes_extractor_item_version_unique"
          |DO UPDATE SET
          |    "impure" = "excluded"."impure"
          |  , "normal" = "excluded"."normal"
          |  , "pure"   = "excluded"."pure"
          |""".stripMargin
      ).contramap {
        case ( extractorType, itemId, distrib ) => ( extractorType, itemId, distrib, modelVersionId )
      }

    def upsertModelVersion( version: ModelVersion ): Update0 =
      // language=SQL
      sql"""INSERT INTO "model_versions"
           |( "family", "version", "description" )
           |VALUES
           |  ( 'SATISFACTORY' :: T_MODEL_FAMILY, ${version.version}, ${version.name} )
           |ON CONFLICT ON CONSTRAINT "model_family_version_unique"
           |DO UPDATE SET
           |    "description" = "excluded"."description"
           |""".stripMargin //
      .update

    def deleteUnusedExtractionRecipes[F[_]: Reducible](
        modelVersionId: ModelVersionId,
        itemPurities: Option[F[( ItemId, ResourcePurity, RecipeId )]]
    ): Update0 =
      // language=SQL
      sql"""DELETE
           |FROM  "extraction_recipes" r
           |USING "items"              i
           |WHERE i."id" = r."item_id"
           |  AND i."model_version_id" = $modelVersionId
           |  ${andOpt( itemPurities.map( notIn( fr"""(r."item_id", r."purity", r."recipe_id")""", _ ) ) )}
           |""".stripMargin //
      .update

    def deleteUnusedResourceNodes[F[_]: Reducible](
        modelVersionId: ModelVersionId,
        extractorItems: Option[F[( ExtractorType, ItemId )]]
    ): Update0 =
      // language=SQL
      sql"""DELETE
           |FROM  "resource_nodes" n
           |USING "items"          i
           |WHERE i."id" = n."item_id"
           |  AND i."model_version_id" = $modelVersionId
           |  ${andOpt( extractorItems.map( notIn( fr"""(n."extractor_type", n."item_id")""", _ ) ) )}
           |""".stripMargin //
      .update

    def deleteUnusedRecipeIngredients[F[_]: Reducible](
        modelVersionId: ModelVersionId,
        ingredientIds: Option[F[RecipeIngredientId]]
    ): Update0 =
      // language=SQL
      sql"""DELETE
           |FROM  "recipe_ingredients" x
           |USING "items"              i
           |WHERE i."id" = x."item_id"
           |  AND i."model_version_id" = $modelVersionId
           |  ${andOpt( ingredientIds.map( Fragments.notIn( fr0"""x."id" """, _ ) ) )}
           |""".stripMargin //
      .update

    def deleteUnusedRecipeProducts[F[_]: Reducible](
        modelVersionId: ModelVersionId,
        productIds: Option[F[RecipeProductId]]
    ): Update0 =
      // language=SQL
      sql"""DELETE
           |FROM  "recipe_products" x
           |USING "items"              i
           |WHERE i."id" = x."item_id"
           |  AND i."model_version_id" = $modelVersionId
           |  ${andOpt( productIds.map( Fragments.notIn( fr0"""x."id" """, _ ) ) )}
           |""".stripMargin //
      .update

    def deleteUnusedRecipes[F[_]: Reducible](
        modelVersionId: ModelVersionId,
        recipeIds: Option[F[RecipeId]]
    ): Update0 =
      // language=SQL
      sql"""DELETE FROM "recipes"
           |WHERE "model_version_id" = $modelVersionId
           |  ${andOpt( recipeIds.map( Fragments.notIn( fr0""""id" """, _ ) ) )}
           |""".stripMargin //
      .update

    def deleteUnusedItems[F[_]: Reducible](
        modelVersionId: ModelVersionId,
        itemIds: Option[F[ItemId]]
    ): Update0 =
      // language=SQL
      sql"""DELETE FROM "items"
           |WHERE "model_version_id" = $modelVersionId
           |  ${andOpt( itemIds.map( Fragments.notIn( fr0""""id" """, _ ) ) )}
           |""".stripMargin //
      .update

    def deleteUnusedMachines[F[_]: Reducible](
        modelVersionId: ModelVersionId,
        machineIds: Option[F[MachineId]]
    ): Update0 =
      // language=SQL
      sql"""DELETE FROM "machines"
           |WHERE "model_version_id" = $modelVersionId
           |  ${andOpt( machineIds.map( Fragments.notIn( fr0""""id" """, _ ) ) )}
           |""".stripMargin //
      .update

  }

  import statements._

  private def writeItems( items: Vector[Item], version: ModelVersionId ): ConnectionIO[Map[ClassName, ItemId]] =
    insertItem( version )
      .updateManyWithGeneratedKeys[ItemId]( "id" )( items )
      .compile
      .toVector
      .map( ids => items.map( _.className ).zip( ids ).toMap )

  private def writeMachines(
      machines: Vector[Machine],
      version: ModelVersionId
  ): ConnectionIO[Map[ClassName, MachineId]] =
    insertMachine( version )
      .updateManyWithGeneratedKeys[MachineId]( "id" )( machines )
      .compile
      .toVector
      .map( ids => machines.map( _.className ).zip( ids ).toMap )

  private def writeRecipes(
      recipes: Vector[Recipe],
      machines: Map[ClassName, MachineId],
      itemIds: Map[ClassName, ItemId],
      version: ModelVersionId
  ): ConnectionIO[( Map[ClassName, RecipeId], Vector[RecipeIngredientId], Vector[RecipeProductId] )] = {
    val recipesWithIds = recipes.flatMap( recipe => machines.get( recipe.producedIn.className ).tupleLeft( recipe ) )

    for {
      recipeIds <- insertRecipe( version )
                    .updateManyWithGeneratedKeys[RecipeId]( "id" )( recipesWithIds )
                    .compile
                    .toVector
                    .map( recipesWithIds.map( _._1 ).zip )
      ingredientIds <- writeRecipeIngredients( recipeIds, itemIds )
      productIds    <- writeRecipeProducts( recipeIds, itemIds )
    } yield ( recipeIds.map { case ( recipe, id ) => ( recipe.className, id ) }.toMap, ingredientIds, productIds )

  }

  private def writeRecipeIngredients(
      recipes: Vector[( Recipe, RecipeId )],
      itemIds: Map[ClassName, ItemId]
  ): ConnectionIO[Vector[RecipeIngredientId]] =
    insertRecipeIngredient
      .updateManyWithGeneratedKeys[RecipeIngredientId]( "id" )(
        recipes.flatMap {
          case ( recipe, id ) =>
            recipe.ingredients.flatMap( _.traverse( item => itemIds.get( item.className ) ) ).toVector.map( ( id, _ ) )
        }
      )
      .compile
      .toVector

  private def writeRecipeProducts(
      recipes: Vector[( Recipe, RecipeId )],
      itemIds: Map[ClassName, ItemId]
  ): ConnectionIO[Vector[RecipeProductId]] =
    insertRecipeProduct
      .updateManyWithGeneratedKeys[RecipeProductId]( "id" )(
        recipes.flatMap {
          case ( recipe, id ) =>
            recipe.products.toList
              .flatMap( _.traverse( item => itemIds.get( item.className ) ) )
              .toVector
              .map( ( id, _ ) )
        }
      )
      .compile
      .toVector

  private def writeExtractionRecipes(
      extractionRecipes: Vector[( Item, ResourcePurity, Recipe )],
      recipeIds: Map[ClassName, RecipeId],
      itemIds: Map[ClassName, ItemId]
  ): ConnectionIO[Unit] =
    insertExtractionRecipe
      .updateMany(
        extractionRecipes.flatMap {
          case ( item, purity, recipe ) =>
            ( itemIds.get( item.className ), recipeIds.get( recipe.className ) ).mapN( ( _, purity, _ ) )
        }
      )
      .void

  private def writeResourceNodes(
      resourceNodes: Map[ExtractorType, Map[ClassName, ResourceDistrib]],
      itemIds: Map[ClassName, ItemId],
      version: ModelVersionId
  ): ConnectionIO[Unit] =
    insertResourceNodes( version )
      .updateMany(
        resourceNodes.toVector.flatMap {
          case ( ex, map ) =>
            map.toVector.flatMap { case ( item, distrib ) => itemIds.get( item ).map( ( ex, _, distrib ) ) }
        }
      )
      .void

  private def pruneModel(
      modelVersionId: ModelVersionId,
      itemIds: Map[ClassName, ItemId],
      machineIds: Map[ClassName, MachineId],
      recipeIds: Map[ClassName, RecipeId],
      ingredientIds: Vector[RecipeIngredientId],
      productIds: Vector[RecipeProductId],
      model: Model
  ): ConnectionIO[Unit] =
    for {
      _ <- deleteUnusedRecipeIngredients( modelVersionId, ingredientIds.toNev ).run
      _ <- deleteUnusedRecipeProducts( modelVersionId, productIds.toNev ).run
      _ <- deleteUnusedExtractionRecipes(
            modelVersionId,
            model.extractionRecipes.flatMap {
              case ( item, purity, recipe ) =>
                ( itemIds.get( item.className ), recipeIds.get( recipe.className ) ).mapN( ( _, purity, _ ) )
            }.toNev
          ).run
      _ <- deleteUnusedResourceNodes(
            modelVersionId,
            model.defaultResourceOptions.resourceNodes.toVector.flatMap {
              case ( extractorType, byItem ) =>
                byItem.toVector
                  .flatMap { case ( item, _ ) => itemIds.get( item ) }
                  .tupleLeft( extractorType )
            }.toNev
          ).run
      _ <- deleteUnusedRecipes( modelVersionId, recipeIds.values.toVector.toNev ).run
      _ <- deleteUnusedMachines( modelVersionId, machineIds.values.toVector.toNev ).run
      _ <- deleteUnusedItems( modelVersionId, itemIds.values.toVector.toNev ).run
    } yield ()

  def writeModel( model: Model, version: ModelVersion ): ConnectionIO[ModelVersionId] = {
    val allRecipes  = model.manufacturingRecipes ++ model.extractionRecipes.map( _._3 )
    val allMachines = allRecipes.map( _.producedIn ).distinctBy( _.className )
    for {
      modelVersionId                           <- upsertModelVersion( version ).withUniqueGeneratedKeys[ModelVersionId]( "id" )
      itemIds                                  <- writeItems( model.items.values.toVector, modelVersionId )
      machineIds                               <- writeMachines( allMachines, modelVersionId )
      ( recipeIds, ingredientIds, productIds ) <- writeRecipes( allRecipes, machineIds, itemIds, modelVersionId )
      _                                        <- writeExtractionRecipes( model.extractionRecipes, recipeIds, itemIds )
      _                                        <- writeResourceNodes( model.defaultResourceOptions.resourceNodes, itemIds, modelVersionId )
      _                                        <- pruneModel( modelVersionId, itemIds, machineIds, recipeIds, ingredientIds, productIds, model )
    } yield modelVersionId
  }

}
