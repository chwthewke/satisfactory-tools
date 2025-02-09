package net.chwthewke.satisfactorytools
package persistence
package plans

import cats.syntax.all._
import doobie._
import doobie.implicits._
import doobie.util.invariant.UnexpectedContinuation
import doobie.util.invariant.UnexpectedEnd

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
import prod.SolverInputs
import protocol.InputTab
import protocol.PlanId

object ReadSolverInputs {
  def readPlanInput[D]( planId: PlanId, tab: InputTab.Aux[D] ): ConnectionIO[D] =
    tab match {
      case InputTab.Bill            => getBill( planId )
      case InputTab.Recipes         => getRecipeList( planId )
      case InputTab.Options         => getOptions( planId )
      case InputTab.ResourceOptions => getResourceOptions( planId )
    }

  def getBill( planId: PlanId ): ConnectionIO[Bill] =
    ( readModelIds( planId, ReadModel.readItems ), statements.selectBillItems.toQuery0( planId ).to[Vector] )
      .mapN( ( items, billRows ) => Bill( billRows.mapFilter( _.traverse( items.get ) ) ) )

  def getRecipeList( planId: PlanId ): ConnectionIO[RecipeList] =
    ( readModelIds( planId, ReadModel.readRecipes ), statements.selectRecipeListItems.toQuery0( planId ).to[Vector] )
      .mapN( ( recipes, recipeList ) => RecipeList( recipeList.mapFilter( recipes.get ) ) )

  def getOptions( planId: PlanId ): ConnectionIO[Options] =
    statements.selectOptions
      .toQuery0( planId )
      .unique
      .adaptErr {
        case UnexpectedEnd          => Error( s"No plan options for plan #$planId" )
        case UnexpectedContinuation => Error( s"Multiple plan options for plan #$planId" )
      }

  private def translateMapKeys[I, K, A]( iMap: Map[I, A], kMap: Map[I, K] ): Map[K, A] =
    for {
      ( i, a ) <- iMap
      k        <- kMap.get( i )
    } yield ( k, a )

  def getResourceOptions( planId: PlanId ): ConnectionIO[ResourceOptions] =
    Headers
      .readPlanHeader( planId )
      .foldF( FC.pure( ( Map.empty[ItemId, Item], ResourceOptions.empty ) ) )( header =>
        (
          ReadModel.readItems( header.modelVersionId ),
          ReadModel.readDefaultResourceOptions( header.modelVersionId )
        ).tupled
      )
      .flatMap {
        case ( itemsById, defaultResourceNodes ) =>
          (
            statements.selectResourceNodes.toQuery0( planId ).stream.compile.foldMonoid,
            statements.selectResourceWeights.toQuery0( planId ).stream.compile.foldMonoid
          ).mapN { ( nodes, weights ) =>
            val itemClasses: Map[ItemId, ClassName] = itemsById.fmap( _.className )

            ResourceOptions(
              nodes.fmap( translateMapKeys( _, itemClasses ) ),
              ResourceWeights( translateMapKeys( weights, itemClasses ) )
            ).mergeResourceNodes( defaultResourceNodes.resourceNodes )
          }
      }

  def getSolverInputs( planId: PlanId ): ConnectionIO[SolverInputs] =
    ( getBill( planId ), getRecipeList( planId ), getOptions( planId ), getResourceOptions( planId ) )
      .mapN( SolverInputs( _, _, _, _ ) )

  private[plans] object statements {

    val selectBillItems: Query[PlanId, Countable[Double, ItemId]] =
      Query(
        // language=SQL
        """SELECT
          |    "item_id"
          |  , "amount"
          |FROM "bill_items"
          |WHERE "plan_id" = ?
          |""".stripMargin
      )

    val selectRecipeListItems: Query[PlanId, RecipeId] =
      Query(
        // language=SQL
        """SELECT "recipe_id"
          |FROM "recipe_lists"
          |WHERE "plan_id" = ?
          |""".stripMargin
      )

    val selectOptions: Query[PlanId, Options] =
      Query(
        // language=SQL
        """SELECT
          |    "belt_option"
          |  , "pipe_option"
          |  , "miner_option"
          |  , "clock_speed_option"
          |  , "extractors_option"
          |  , "fracking_option"
          |FROM "plan_options"
          |WHERE "plan_id" = ?
          |""".stripMargin
      )

    val selectResourceNodes: Query[PlanId, Map[ExtractorType, Map[ItemId, ResourceDistrib]]] =
      Query[PlanId, ( ExtractorType, ItemId, ResourcePurity, Int )](
        // language=SQL
        """SELECT
          |    "extractor_type"
          |  , "item_id"
          |  , "purity"
          |  , "amount"
          |FROM "resource_distribution_options"
          |WHERE "plan_id" = ?
          |""".stripMargin
      ).map {
        case ( extractorType, item, purity, amount ) =>
          Map( extractorType -> Map( item -> ResourceDistrib.of( purity, amount ) ) )
      }

    val selectResourceWeights: Query[PlanId, Map[ItemId, Int]] =
      Query[PlanId, ( ItemId, Int )](
        // language=SQL
        """SELECT
          |    "item_id"
          |  , "weight"
          |FROM "resource_weights"
          |WHERE "plan_id" = ?
          |""".stripMargin
      ).map { case ( itemId, amount ) => Map( itemId -> amount ) }

  }
}
