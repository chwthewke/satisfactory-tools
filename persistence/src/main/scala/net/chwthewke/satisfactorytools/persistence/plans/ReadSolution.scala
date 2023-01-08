package net.chwthewke.satisfactorytools
package persistence
package plans

import cats.Foldable
import cats.Order.catsKernelOrderingForOrder
import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.functorFilter._
import cats.syntax.semigroup._
import cats.syntax.traverse._
import doobie._
import doobie.implicits._

import data.Countable
import data.Item
import model.Bill
import model.GroupAssignments
import model.Machine
import model.Recipe
import prod.ClockedRecipe
import prod.Factory
import protocol.CustomGroupResult
import protocol.ItemIO
import protocol.ItemSrcDest
import protocol.OutputTab
import protocol.PlanId
import protocol.SolutionId

object ReadSolution {

  def readPlanResult[D](
      planId: PlanId,
      solutionId: SolutionId,
      outputTab: OutputTab.Aux[D]
  ): ConnectionIO[D] =
    outputTab match {
      case OutputTab.CustomGroup( ix ) => getGroupResult( planId, solutionId, ix )
      case OutputTab.Steps             => getSolution( planId, solutionId )
      case OutputTab.Items             => getItems( planId, solutionId )
      case OutputTab.Machines          => getMachines( planId, solutionId )
      case OutputTab.Inputs            => getRawInputs( planId, solutionId )
    }

  private def getGroupResult( planId: PlanId, solutionId: SolutionId, group: Int ): ConnectionIO[CustomGroupResult] =
    (
      readModelIds( planId, ReadModel.readRecipes ),
      statements.selectGroupManufacturingRecipes.toQuery0( ( solutionId, group ) ).to[Vector]
    ).mapN {
      case ( recipesById, groupRecipes ) =>
        val recipes: Vector[Countable[Double, Recipe]] =
          groupRecipes.mapFilter( _.traverse( recipesById.get ) )

        val external = recipes
          .foldMap( _.flatTraverse( _.itemsPerMinute ) )
          .gather

        val ( input, output ) = external.partition( _.amount < 0 )

        val factory = Factory(
          Vector.empty,
          recipes,
          Countable( input, -1d ).flatSequence,
          output
        )

        CustomGroupResult(
          group,
          factory,
          extractItemIO( Bill.empty, factory, ItemSrcDest.Output ),
          extractMachines( factory )
        )

    }

  private def getSolution( planId: PlanId, solutionId: SolutionId ): ConnectionIO[( Factory, GroupAssignments )] =
    (
      readModelIds( planId, ReadModel.readItems ),
      readModelIds( planId, ReadModel.readRecipes ),
      statements.selectExtractionRecipes.toQuery0( solutionId ).to[Vector],
      statements.selectManufacturingRecipes.toQuery0( solutionId ).to[Vector],
      statements.selectExtraInputs.toQuery0( solutionId ).to[Vector],
      statements.selectExtraOutputs.toQuery0( solutionId ).to[Vector]
    ).mapN {
      case ( itemsById, recipesById, extraction, manufacturing, inputs, outputs ) =>
        val extractionRecipes: Vector[ClockedRecipe] =
          extraction.mapFilter {
            case ( r, s ) =>
              r.traverse( recipesById.get ).map( ClockedRecipe.overclocked( _, s ) )
          }

        val manufacturingRecipesWithGroups: Vector[( Countable[Double, Recipe], Option[Int] )] =
          manufacturing.mapFilter {
            case ( r, g ) =>
              r.traverse( recipesById.get ).tupleRight( g )
          }

        val groups: GroupAssignments = GroupAssignments(
          manufacturingRecipesWithGroups.flatMap { case ( r, g ) => g.tupleLeft( r.item.className ) }.toMap
        )

        val extraInputs: Vector[Countable[Double, Item]] =
          inputs.mapFilter( _.traverse( itemsById.get ) )

        val extraOutputs: Vector[Countable[Double, Item]] =
          outputs.mapFilter( _.traverse( itemsById.get ) )

        (
          Factory(
            extractionRecipes,
            manufacturingRecipesWithGroups.map( _._1 ),
            extraInputs,
            extraOutputs
          ),
          groups
        )
    }

  // TODO see if these can be optimized with purpose-specific SQL
  //   but it's not exactly highest priority
  private def getItems( planId: PlanId, solutionId: SolutionId ): ConnectionIO[Map[Item, ItemIO]] =
    (
      ReadSolverInputs.getBill( planId ),
      getSolution( planId, solutionId ).map( _._1 )
    ).mapN( extractItemIO( _, _, ItemSrcDest.Byproduct ) )

  private def extractItemIO( bill: Bill, factory: Factory, extraOutput: ItemSrcDest ): Map[Item, ItemIO] =
    factory.allRecipes.foldMap( itemInOut ) |+|
      itemOut( ItemSrcDest.Requested, bill.items ) |+|
      itemIn( ItemSrcDest.Input, factory.extraInputs ) |+|
      itemOut( extraOutput, factory.extraOutputs )

  private def itemInOut( recipeBlock: ClockedRecipe ): Map[Item, ItemIO] = {
    val key = ItemSrcDest.Step( recipeBlock.recipe.item )

    itemOut( key, recipeBlock.ingredientsPerMinute ) |+|
      itemIn( key, recipeBlock.productsPerMinute )
  }

  private def itemIn[F[_]: Foldable](
      key: ItemSrcDest,
      items: F[Countable[Double, Item]]
  ): Map[Item, ItemIO] =
    items.foldMap( c => Map( c.item -> ItemIO.in( key, c.amount ) ) )

  private def itemOut[F[_]: Foldable](
      key: ItemSrcDest,
      items: F[Countable[Double, Item]]
  ): Map[Item, ItemIO] =
    items.foldMap( c => Map( c.item -> ItemIO.out( key, c.amount ) ) )

  private def getMachines( planId: PlanId, solutionId: SolutionId ): ConnectionIO[Vector[Countable[Int, Machine]]] =
    getSolution( planId, solutionId )
      .map( _._1 )
      .map( extractMachines )

  private def extractMachines( factory: Factory ): Vector[Countable[Int, Machine]] =
    factory.allRecipes
      .map( r => r.recipe.as( r.machine ) )
      .gather
      .sortBy( m => ( m.item.machineType, m.item.powerConsumption ) )

  private def getRawInputs( planId: PlanId, solutionId: SolutionId ): ConnectionIO[Vector[Countable[Double, Item]]] =
    getSolution( planId, solutionId )
      .map( _._1 )
      .map( extractRawInputs )

  private def extractRawInputs( factory: Factory ): Vector[Countable[Double, Item]] =
    factory.extraction.map( _.productsPerMinute.head ).gather

  private[plans] object statements {
    val selectGroupManufacturingRecipes: Query[( SolutionId, Int ), Countable[Double, RecipeId]] =
      Query(
        // language=SQL
        """SELECT
          |    "recipe_id"
          |  , "amount"
          |FROM "solution_manufacturing_recipes"
          |WHERE "solution_id" = ?
          |  AND "custom_group" = ?
          |ORDER BY "group_order"
          |""".stripMargin
      )

    val selectManufacturingRecipes: Query[SolutionId, ( Countable[Double, RecipeId], Option[Int] )] =
      Query(
        // language=SQL
        """SELECT
          |    "recipe_id"
          |  , "amount"
          |  , "custom_group"
          |FROM "solution_manufacturing_recipes"
          |WHERE "solution_id" = ?
          |ORDER BY "id"
          |""".stripMargin
      )

    val selectExtractionRecipes: Query[SolutionId, ( Countable[Int, RecipeId], Double )] =
      Query(
        // language=SQL
        """SELECT
          |    "recipe_id"
          |  , "amount"
          |  , "clock_speed"
          |FROM "solution_extraction_recipes"
          |WHERE "solution_id" = ?
          |ORDER BY "id"
          |""".stripMargin
      )

    val selectExtraInputs: Query[SolutionId, Countable[Double, ItemId]] =
      Query(
        // language=SQL
        """SELECT
          |    "item_id"
          |  , "amount"
          |FROM "solution_extra_inputs"
          |WHERE "solution_id" = ?
          |""".stripMargin
      )

    val selectExtraOutputs: Query[SolutionId, Countable[Double, ItemId]] =
      Query(
        // language=SQL
        """SELECT
          |    "item_id"
          |  , "amount"
          |FROM "solution_extra_outputs"
          |WHERE "solution_id" = ?
          |""".stripMargin
      )

  }

}
