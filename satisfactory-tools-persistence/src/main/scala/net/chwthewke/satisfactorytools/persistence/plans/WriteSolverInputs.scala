package net.chwthewke.satisfactorytools
package persistence
package plans

import alleycats.std.iterable._
import cats.syntax.apply._
import cats.syntax.functor._
import cats.syntax.functorFilter._
import cats.syntax.traverse._
import doobie._

import data.ClassName
import data.Countable
import data.Item
import model.Bill
import model.ExtractorType
import model.Options
import model.RecipeList
import model.ResourceDistrib
import model.ResourceOptions
import model.ResourcePurity
import model.ResourceWeights
import protocol.PlanId

object WriteSolverInputs {

  def updateBill( planId: PlanId, bill: Bill ): ConnectionIO[Unit] =
    statements.deleteBill.run( planId ) *> insertBill( planId, bill )

  private def insertBill( planId: PlanId, bill: Bill ): ConnectionIO[Unit] =
    ReadModel.readItemIds.flatMap { itemIds =>
      val rows = bill.items
        .mapFilter( _.traverse( it => itemIds.get( it.className ) ) )
        .tupleLeft( planId )

      statements.insertBill.updateMany( rows )
    }.void

  def updateRecipeList( planId: PlanId, recipeList: RecipeList ): ConnectionIO[Unit] =
    statements.deleteRecipeList.run( planId ) *> insertRecipeList( planId, recipeList )

  private def insertRecipeList( planId: PlanId, recipeList: RecipeList ): ConnectionIO[Unit] =
    ReadModel.readRecipeIds.flatMap { recipeIds =>
      val rows: Vector[( PlanId, RecipeId )] =
        recipeList.recipes
          .mapFilter( re => recipeIds.get( re.className ) )
          .tupleLeft( planId )

      statements.insertRecipeList.updateMany( rows )
    }.void

  def setDefaultRecipeList( planId: PlanId ): ConnectionIO[Unit] =
    statements.insertDefaultRecipeList.run( planId ).void

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
                itemIds.get( item.className ).map( ( planId, _, amount ) )
            }
        )
        .void

  private def updateResourceDistribution(
      planId: PlanId,
      itemIds: Map[ClassName, ItemId],
      resourceDistribOptions: Map[ExtractorType, Map[Item, ResourceDistrib]]
  ): ConnectionIO[Unit] = {
    val rows: Iterable[( PlanId, ExtractorType, ItemId, ResourcePurity, Int )] = for {
      ( extractor, dists ) <- resourceDistribOptions
      ( item, dist )       <- dists
      ( purity, amount ) <- Vector(
                             ( ResourcePurity.Impure, dist.impureNodes ),
                             ( ResourcePurity.Normal, dist.normalNodes ),
                             ( ResourcePurity.Pure, dist.pureNodes )
                           )
      itemId <- itemIds.get( item.className )
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

    val insertDefaultRecipeList: Update[PlanId] =
      Update[( PlanId, Int )](
        // language=SQL
        """INSERT INTO "recipe_lists"
          |  ( "plan_id", "recipe_id" )
          |  SELECT
          |      ?
          |    , "id"
          |  FROM "recipes"
          |  WHERE "data_version" = ?
          |""".stripMargin
      ).contramap( ( _, ModelVersion ) )

  }
}
