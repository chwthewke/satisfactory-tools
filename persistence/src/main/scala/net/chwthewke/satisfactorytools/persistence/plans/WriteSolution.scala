package net.chwthewke.satisfactorytools
package persistence.plans

import cats.syntax.all._
import doobie._
import doobie.implicits._
import doobie.util.invariant.UnexpectedEnd

import data.ClassName
import data.Countable
import data.Item
import model.Recipe
import persistence.ItemId
import persistence.ReadModel
import persistence.RecipeId
import persistence.plans
import prod.ClockedRecipe
import prod.Factory
import prod.GroupAssignments
import protocol.ModelVersionId
import protocol.PlanId
import protocol.SolutionId

object WriteSolution {

  def clearSolution( planId: PlanId ): doobie.ConnectionIO[Unit] = statements.deleteSolution.run( planId ).void

  def writeSolution( planId: PlanId, solution: Either[String, Factory] ): ConnectionIO[Unit] =
    Headers
      .readPlanHeader( planId )
      .semiflatMap( header =>
        for {
          customGroups <- plans.CustomGroups.readPlanCustomGroups( header )
          _            <- clearSolution( planId )
          _ <- solution.fold(
                 err => statements.insertSolutionError.run( ( planId, err ) ),
                 factory => writeSolutionWithGroups( planId, header.modelVersionId, factory, customGroups )
               )
        } yield ()
      )
      .getOrElse( () )

  private def writeSolutionWithGroups(
      planId: PlanId,
      modelVersionId: ModelVersionId,
      factory: Factory,
      customGroups: GroupAssignments[RecipeId]
  ): ConnectionIO[Unit] =
    for {
      solutionId <- writeSolutionHeader( planId, customGroups.count )
      itemIds    <- ReadModel.readItemIds( modelVersionId )
      recipeIds  <- ReadModel.readRecipeIds( modelVersionId )
      _          <- writeExtractionRecipes( solutionId, recipeIds, factory.extraction )
      _          <- writeManufacturingRecipes( solutionId, customGroups, factory.manufacturing, recipeIds )
      _          <- writeExtraInputs( solutionId, itemIds, factory.extraInputs )
      _          <- writeExtraOutputs( solutionId, itemIds, factory.extraOutputs )
    } yield ()

  private def writeSolutionHeader( planId: PlanId, customGroupCount: Int ): ConnectionIO[SolutionId] =
    statements.insertSolution
      .withUniqueGeneratedKeys[SolutionId]( "id" )( ( planId, customGroupCount ) )
      .adaptErr {
        case UnexpectedEnd => Error( s"Unable to create solution for plan #$planId" )
      }

  private def writeExtractionRecipes(
      solutionId: SolutionId,
      recipeIds: Map[ClassName, RecipeId],
      recipes: Vector[ClockedRecipe]
  ): ConnectionIO[Int] = {

    val rows: Vector[( SolutionId, RecipeId, Int, Double )] = recipes.flatMap {
      case ClockedRecipe( recipe, clockSpeed, machineCount ) =>
        recipeIds.get( recipe.item.className ).map( ( solutionId, _, machineCount, clockSpeed ) )
    }

    statements.insertSolutionExtractionRecipe.updateMany( rows )
  }

  def writeManufacturingRecipes(
      solutionId: SolutionId,
      groups: GroupAssignments[RecipeId],
      recipes: Vector[Countable[Double, Recipe]],
      recipeIds: Map[ClassName, RecipeId]
  ): doobie.ConnectionIO[Int] =
    writeManufacturingRecipesIds(
      solutionId,
      groups,
      recipes.mapFilter( _.traverse( r => recipeIds.get( r.className ) ) )
    )

  private[persistence] def writeManufacturingRecipesIds(
      solutionId: SolutionId,
      customGroups: GroupAssignments[RecipeId],
      recipes: Vector[Countable[Double, RecipeId]]
  ): ConnectionIO[Int] = {

    val groupColumns: Map[RecipeId, ( Int, Int, Boolean )] = customGroups.groupIndices

    val rows: Vector[( SolutionId, Countable[Double, RecipeId], Option[( Int, Int )], Boolean )] =
      recipes.map { recipeId =>
        val groupCols: Option[( Int, Int, Boolean )] = groupColumns.get( recipeId.item )
        ( solutionId, recipeId, groupCols.map { case ( a, b, _ ) => ( a, b ) }, groupCols.fold( false )( _._3 ) )
      }

    statements.insertSolutionManufacturingRecipe.updateMany( rows )
  }

  private def writeExtraInputs(
      solutionId: SolutionId,
      itemIds: Map[ClassName, ItemId],
      extraInputs: Vector[Countable[Double, Item]]
  ): ConnectionIO[Int] =
    writeExtraInputOutputs( statements.insertSolutionExtraInput )( solutionId, itemIds, extraInputs )

  private def writeExtraOutputs(
      solutionId: SolutionId,
      itemIds: Map[ClassName, ItemId],
      extraOutputs: Vector[Countable[Double, Item]]
  ): ConnectionIO[Int] =
    writeExtraInputOutputs( statements.insertSolutionExtraOutput )( solutionId, itemIds, extraOutputs )

  private def writeExtraInputOutputs( stmt: Update[( SolutionId, ItemId, Double )] )(
      solutionId: SolutionId,
      itemIds: Map[ClassName, ItemId],
      items: Vector[Countable[Double, Item]]
  ): ConnectionIO[Int] = {
    val rows: Vector[( SolutionId, ItemId, Double )] =
      items.mapFilter {
        case Countable( item, amount ) =>
          itemIds.get( item.className ).map( ( solutionId, _, amount ) )
      }

    stmt.updateMany( rows )
  }

  private[plans] object statements {
    val deleteSolution: Update[PlanId] =
      Update(
        // language=SQL
        """DELETE FROM "plan_solutions"
          |WHERE "plan_id" = ?
          |""".stripMargin
      )

    val insertSolution: Update[( PlanId, Int )] =
      Update(
        // language=SQL
        """INSERT INTO "plan_solutions"
          |  ( "plan_id", "custom_groups" )
          |VALUES
          |  ( ?, ? )
          |""".stripMargin
      )

    val insertSolutionError: Update[( PlanId, String )] =
      Update(
        // language=SQL
        """INSERT INTO "plan_solutions"
          |  ( "plan_id", "error_message", "custom_groups" )
          |VALUES
          |  ( ?, ?, 0 )
          |""".stripMargin
      )

    val insertSolutionExtractionRecipe: Update[( SolutionId, RecipeId, Int, Double )] =
      Update(
        // language=SQL
        """INSERT INTO "solution_extraction_recipes"
          |  ( "solution_id", "recipe_id", "amount", "clock_speed" )
          |VALUES
          |  ( ?, ?, ?, ? )
          |""".stripMargin
      )

    val insertSolutionManufacturingRecipe
        : Update[( SolutionId, Countable[Double, RecipeId], Option[( Int, Int )], Boolean )] =
      Update(
        // language=SQL
        """INSERT INTO "solution_manufacturing_recipes"
          |  ( "solution_id", "recipe_id", "amount", "custom_group", "group_order", "section_before" )
          |VALUES
          |  ( ?, ?, ?, ?, ?, ? )
          |ON CONFLICT ON CONSTRAINT "solution_manufacturing_recipes_unique"
          |DO UPDATE
          |  SET
          |    "amount"         = "excluded"."amount"
          |  , "custom_group"   = "excluded"."custom_group"
          |  , "group_order"    = "excluded"."group_order"
          |  , "section_before" = "excluded"."section_before"
          |""".stripMargin
      )

    val insertSolutionExtraInput: Update[( SolutionId, ItemId, Double )] =
      Update(
        // language=SQL
        """INSERT INTO "solution_extra_inputs"
          |  ( "solution_id", "item_id", "amount" )
          |VALUES
          |  ( ?, ?, ? )
          |""".stripMargin
      )

    val insertSolutionExtraOutput: Update[( SolutionId, ItemId, Double )] =
      Update(
        // language=SQL
        """INSERT INTO "solution_extra_outputs"
          |  ( "solution_id", "item_id", "amount" )
          |VALUES
          |  ( ?, ?, ? )
          |""".stripMargin
      )

  }

}
