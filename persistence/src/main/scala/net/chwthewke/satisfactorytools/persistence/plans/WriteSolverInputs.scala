package net.chwthewke.satisfactorytools
package persistence
package plans

import alleycats.std.iterable._
import cats.data.NonEmptyVector
import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.functorFilter._
import cats.syntax.traverse._
import cats.syntax.vector._
import doobie._
import doobie.implicits._

import data.ClassName
import data.Countable
import model.Bill
import model.ExtractorType
import model.Model
import model.Options
import model.Recipe
import model.RecipeList
import model.ResourceDistrib
import model.ResourceOptions
import model.ResourcePurity
import model.ResourceWeights
import prod.Factory
import protocol.PlanId

object WriteSolverInputs {

  def updateBill( planId: PlanId, bill: Bill ): ConnectionIO[Unit] =
    statements.deleteBill.run( planId ) *> insertBill( planId, bill )

  private def insertBill( planId: PlanId, bill: Bill ): ConnectionIO[Unit] =
    readModelIds( planId, ReadModel.readItemIds ).flatMap { itemIds =>
      val rows = bill.items
        .mapFilter( _.traverse( it => itemIds.get( it.className ) ) )
        .tupleLeft( planId )

      statements.insertBill.updateMany( rows )
    }.void

  def updateRecipeList( planId: PlanId, recipeList: RecipeList ): ConnectionIO[Unit] =
    statements.deleteRecipeList.run( planId ) *> insertRecipeList( planId, recipeList )

  private def insertRecipeList( planId: PlanId, recipeList: RecipeList ): ConnectionIO[Unit] =
    readModelIds( planId, ReadModel.readRecipeIds ).flatMap { recipeIds =>
      val rows: Vector[( PlanId, RecipeId )] =
        recipeList.recipes
          .mapFilter( re => recipeIds.get( re.className ) )
          .tupleLeft( planId )

      statements.insertRecipeList.updateMany( rows )
    }.void

  def setDefaultRecipeList( planId: PlanId ): ConnectionIO[Unit] =
    statements.insertDefaultRecipeList( planId ).run.void

  def lockCurrentRecipeList( planId: PlanId, model: Model, factory: Factory ): ConnectionIO[Unit] = {
    val factoryRecipes: Vector[Recipe] = factory.manufacturingRecipes.map( _.recipe.item )
    val producedItems: Set[ClassName]  = factoryRecipes.map( _.products.head.item.className ).toSet
    val usedRecipes: Set[ClassName]    = factoryRecipes.map( _.className ).toSet

    val recipesToDelete: Vector[Recipe] =
      model.manufacturingRecipes.filter( recipe =>
        producedItems.contains( recipe.products.head.item.className ) &&
          !usedRecipes.contains( recipe.className )
      )

    readModelIds( planId, ReadModel.readRecipeIds ).flatMap( recipeIds =>
      recipesToDelete
        .mapFilter( recipe => recipeIds.get( recipe.className ) )
        .toNev
        .traverse_( statements.deleteUnusedRecipes( planId, _ ).run )
    )
  }

  def removeMatterConversionFromRecipeList( planId: PlanId, model: Model ): ConnectionIO[Unit] =
    model.manufacturingRecipes
      .filter( _.isMatterConversion )
      .map( _.className )
      .toNev
      .traverse_( statements.deleteFromRecipeListByClassName( planId, _ ).run )

  def addAllAlternatesToRecipeList( planId: PlanId ): ConnectionIO[Unit] =
    statements.insertAllAlternatesToRecipeList( planId ).run.void

  def removeAllAlternatesFromRecipeList( planId: PlanId ): ConnectionIO[Unit] =
    statements.deleteAllAlternatesFromRecipeList( planId ).run.void

  def addRecipesUpToTier( planId: PlanId, tier: Int, alternates: Boolean ): ConnectionIO[Unit] =
    statements.clearRecipeList( planId ).run *>
      statements.insertToRecipeList( planId, tier, alternates ).run.void

  def updateOptions( planId: PlanId, options: Options ): ConnectionIO[Unit] =
    statements.upsertOptions.run( ( planId, options ) ).void

  def updateResourceOptions(
      planId: PlanId,
      itemIds: Map[ClassName, ItemId],
      resourceOptions: ResourceOptions
  ): ConnectionIO[Unit] =
    updateResourceWeights( planId, itemIds, resourceOptions.resourceWeights ) *>
      updateResourceDistribution( planId, itemIds, resourceOptions.resourceNodes )

  private def updateResourceWeights(
      planId: PlanId,
      itemIds: Map[ClassName, ItemId],
      resourceWeights: ResourceWeights
  ): ConnectionIO[Unit] =
    statements.deleteResourceWeights.run( planId ) *>
      statements.insertResourceWeights
        .updateMany(
          resourceWeights.weights.toVector
            .mapFilter {
              case ( item, amount ) =>
                itemIds.get( item ).map( ( planId, _, amount ) )
            }
        )
        .void

  private def updateResourceDistribution(
      planId: PlanId,
      itemIds: Map[ClassName, ItemId],
      resourceDistribOptions: Map[ExtractorType, Map[ClassName, ResourceDistrib]]
  ): ConnectionIO[Unit] = {
    val rows: Iterable[( PlanId, ExtractorType, ItemId, ResourcePurity, Int )] = for {
      ( extractor, dists ) <- resourceDistribOptions
      ( item, dist )       <- dists
      ( purity, amount ) <- Vector(
                              ( ResourcePurity.Impure, dist.impureNodes ),
                              ( ResourcePurity.Normal, dist.normalNodes ),
                              ( ResourcePurity.Pure, dist.pureNodes )
                            )
      itemId <- itemIds.get( item )
    } yield ( planId, extractor, itemId, purity, amount )

    statements.upsertResourceDistribution.updateMany( rows ).void
  }

  private[plans] object statements {
    val upsertOptions: Update[( PlanId, Options )] =
      Update(
        // language=SQL
        """INSERT INTO "plan_options"
          |  ( "plan_id"
          |  , "belt_option"
          |  , "pipe_option"
          |  , "miner_option"
          |  , "clock_speed_option"
          |  , "extractors_option"
          |  , "fracking_option"
          |  )
          |VALUES
          |  ( ?, ?, ?, ?, ?, ?, ? )
          |ON CONFLICT ON CONSTRAINT "plan_option_unique"
          |DO UPDATE SET
          |    "belt_option"        = "excluded"."belt_option"
          |  , "pipe_option"        = "excluded"."pipe_option"
          |  , "miner_option"       = "excluded"."miner_option"
          |  , "clock_speed_option" = "excluded"."clock_speed_option"
          |  , "extractors_option"  = "excluded"."extractors_option"
          |  , "fracking_option"    = "excluded"."fracking_option"
          |""".stripMargin
      )

    val upsertResourceDistribution: Update[( PlanId, ExtractorType, ItemId, ResourcePurity, Int )] =
      Update(
        // language=SQL
        """INSERT INTO "resource_distribution_options"
          |  ( "plan_id"
          |  , "extractor_type"
          |  , "item_id"
          |  , "purity"
          |  , "amount"
          |  )
          |VALUES
          |  ( ?, ?, ?, ?, ? )
          |ON CONFLICT ON CONSTRAINT "resource_distribution_unique"
          |DO UPDATE SET
          |    "amount" = "excluded"."amount"
          |""".stripMargin
      )

    val deleteResourceWeights: Update[PlanId] =
      Update(
        // language=SQL
        """DELETE FROM "resource_weights"
          |WHERE "plan_id" = ?
          |""".stripMargin
      )

    val insertResourceWeights: Update[( PlanId, ItemId, Int )] =
      Update(
        // language=SQL
        """INSERT INTO "resource_weights"
          |  ( "plan_id"
          |  , "item_id"
          |  , "weight"
          |  )
          |VALUES
          |  ( ?, ?, ? )
          |""".stripMargin
      )

    val deleteBill: Update[PlanId] =
      Update(
        // language=SQL
        """DELETE FROM "bill_items"
          |WHERE "plan_id" = ?
          |""".stripMargin
      )

    val insertBill: Update[( PlanId, Countable[Double, ItemId] )] =
      Update(
        // language=SQL
        """INSERT INTO "bill_items"
          |  ( "plan_id", "item_id", "amount" )
          |VALUES
          |  ( ?, ?, ? )
          |""".stripMargin
      )

    val deleteRecipeList: Update[PlanId] =
      Update(
        // language=SQL
        """DELETE FROM "recipe_lists"
          |WHERE "plan_id" = ?
          |""".stripMargin
      )

    val insertRecipeList: Update[( PlanId, RecipeId )] =
      Update(
        // language=SQL
        """INSERT INTO "recipe_lists"
          |  ( "plan_id", "recipe_id" )
          |VALUES
          |  ( ?, ? )
          |""".stripMargin
      )

    private def insertAllRecipes( condition: Fragment, moreConditions: Fragment* ): Update0 =
      // language=SQL
      sql"""INSERT INTO "recipe_lists"
           |  ( "plan_id", "recipe_id" )
           |  SELECT
           |      p."id"
           |    , r."id"
           |  FROM       "plans"              p
           |  INNER JOIN "recipes"            r ON r."model_version_id" = p."model_version_id"
           |  LEFT  JOIN "extraction_recipes" x on r."id" = x."recipe_id"
           |  ${Fragments.whereAnd( NonEmptyVector( condition, moreConditions.toVector ) )}
           |ON CONFLICT ON CONSTRAINT "recipe_list_unique"
           |DO NOTHING
           |""".stripMargin //
        .update

    private def withPlanId( planId: PlanId ): Fragment =
      // language=SQL
      fr0"""p."id" = $planId"""

    private val notExtractionRecipe: Fragment =
      // language=SQL
      fr0"""x."recipe_id" IS NULL"""

    private val isAlternate: Fragment =
      // language=SQL
      fr0"""r."display_name" ILIKE 'alternate:%'"""

    private val isNotAlternate: Fragment =
      // language=SQL
      fr0"""r."display_name" NOT ILIKE 'alternate:%'"""

    private def isAvailableAtTier( n: Int ): Fragment =
      // language=SQL
      fr0"""r."tier" IS NULL OR r."tier" <= $n"""

    def insertDefaultRecipeList( planId: PlanId ): Update0 =
      insertAllRecipes( withPlanId( planId ), notExtractionRecipe )

    def insertAllAlternatesToRecipeList( planId: PlanId ): Update0 =
      insertAllRecipes( withPlanId( planId ), notExtractionRecipe, isAlternate )

    def deleteFromRecipeListByClassName( planId: PlanId, classNames: NonEmptyVector[ClassName] ): Update0 =
      // language=SQL
      sql"""DELETE FROM "recipe_lists" l
           |USING "recipes" r
           |WHERE l."plan_id" = $planId
           |  AND r."id" = l."recipe_id"
           |  AND ${Fragments.in( fr0"""r."class_name"""", classNames )}
           |""".stripMargin //
        .update

    def deleteAllAlternatesFromRecipeList( planId: PlanId ): Update0 =
      // language=SQL
      sql"""DELETE FROM "recipe_lists" l
           |USING "recipes" r
           |WHERE l."plan_id" = $planId
           |  AND r."id" = l."recipe_id"
           |  AND $isAlternate
           |""".stripMargin //
        .update

    def deleteUnusedRecipes( planId: PlanId, recipeIds: NonEmptyVector[RecipeId] ): Update0 =
      // language=SQL
      sql"""DELETE FROM "recipe_lists"
           |WHERE "plan_id" = $planId
           |  AND ${Fragments.in( fr0""""recipe_id"""", recipeIds )}
           |""".stripMargin.update

    def clearRecipeList( planId: PlanId ): Update0 =
      // language=SQL
      sql"""DELETE FROM "recipe_lists"
           |WHERE "plan_id" = $planId
           |""".stripMargin.update

    def insertToRecipeList( planId: PlanId, maxTier: Int, alternates: Boolean ): Update0 =
      insertAllRecipes(
        withPlanId( planId ),
        Vector( notExtractionRecipe, isAvailableAtTier( maxTier ) )
          ++ Option.when( !alternates )( isNotAlternate ): _*
      )
  }
}
