package net.chwthewke.satisfactorytools
package persistence

import cats.data.OptionT
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import doobie._

import api.PlannerApi
import data.ClassName
import model.Bill
import model.Options
import model.RecipeList
import model.ResourceOptions
import persistence.plans.PlanTrees
import prod.Calculator
import prod.ojsolver.ConstraintSolver
import prod.tree.TreeCommand
import protocol.InputTab
import protocol.ModelVersionId
import protocol.OutputTab
import protocol.PlanHeader
import protocol.PlanId
import protocol.SolutionHeader
import protocol.SolutionId
import protocol.UserId

/*
 *  TODO Models are immutable (per-version),
 *   so it would really be more efficient to cache the Model instances somehow...
 *   maybe even cache them in the web app?
 */
object Plans extends PlannerApi[ConnectionIO] {

  override def newPlan(
      userId: UserId,
      modelVersion: ModelVersionId
  ): ConnectionIO[PlanId] =
    plans.Headers.writeNewPlan( userId, modelVersion )

  override def addCustomGroup( planId: PlanId ): ConnectionIO[Boolean] =
    plans.CustomGroups.updateGroupCountIncrement( planId )

  override def removeCustomGroup( planId: PlanId ): ConnectionIO[Boolean] =
    plans.CustomGroups.updateGroupCountDecrement( planId )
  /*
   * TODO might be able to wrangle a ConnectionIO[Boolean] indicating change or lack thereof
   *  https://stackoverflow.com/questions/41482441/postgresql-upsert-do-nothing-if-fields-dont-change
   */
  override def setBill( planId: PlanId, bill: Bill ): ConnectionIO[Unit] =
    plans.WriteSolverInputs.updateBill( planId, bill )

  override def setRecipeList( planId: PlanId, recipeList: RecipeList ): ConnectionIO[Unit] =
    plans.WriteSolverInputs.updateRecipeList( planId, recipeList )

  override def addAllRecipes( planId: PlanId ): ConnectionIO[Unit] =
    plans.WriteSolverInputs.setDefaultRecipeList( planId )

  override def lockCurrentRecipes( planId: PlanId ): ConnectionIO[Unit] =
    plans.Headers
      .readPlanHeader( planId )
      .mproduct( header =>
        OptionT
          .fromOption[ConnectionIO]( header.solution.valueAndCount )
          .semiflatMap {
            case ( solutionId, groupCount ) =>
              plans.ReadSolution.readPlanResult( planId, solutionId, groupCount, OutputTab.Steps( editGroups = false ) )
          }
          .map( _._1 )
      )
      .flatMap {
        case ( header, factory ) => ReadModel.getModel( header.modelVersionId ).tupleRight( factory )
      }
      .semiflatMap {
        case ( model, factory ) => plans.WriteSolverInputs.lockCurrentRecipeList( planId, model, factory )
      }
      .value
      .void

  override def addAllAlternatesToRecipeList( planId: PlanId ): ConnectionIO[Unit] =
    plans.WriteSolverInputs.addAllAlternatesToRecipeList( planId )

  override def removeAllAlternatesFromRecipeList( planId: PlanId ): ConnectionIO[Unit] =
    plans.WriteSolverInputs.removeAllAlternatesFromRecipeList( planId )

  override def addRecipesUpToTier( planId: PlanId, tier: Int, alternates: Boolean ): ConnectionIO[Unit] =
    plans.WriteSolverInputs.addRecipesUpToTier( planId, tier, alternates )

  override def setOptions( planId: PlanId, options: Options ): ConnectionIO[Unit] =
    plans.WriteSolverInputs.updateOptions( planId, options )

  override def setResourceOptions( planId: PlanId, resourceOptions: ResourceOptions ): ConnectionIO[Unit] =
    plans.Headers
      .readPlanHeader( planId )
      .semiflatMap( header =>
        ReadModel
          .readItemIds( header.modelVersionId )
          .flatMap( plans.WriteSolverInputs.updateResourceOptions( planId, _, resourceOptions ) )
      )
      .getOrElse( () )

  override def setCustomGroupSelection( planId: PlanId, groups: Map[ClassName, Int] ): ConnectionIO[Unit] =
    plans.CustomGroups.updateCustomGroupSelection( planId, groups )

  override def getPlanHeader( planId: PlanId ): OptionT[ConnectionIO, PlanHeader] =
    plans.Headers.readPlanHeader( planId )

  override def getPlanQuery( planId: PlanId, inputTab: InputTab ): ConnectionIO[inputTab.Data] =
    plans.ReadSolverInputs.readPlanInput( planId, inputTab )

  override def getPlanResult(
      planId: PlanId,
      solutionHeader: SolutionHeader[SolutionId],
      outputTab: OutputTab
  ): ConnectionIO[SolutionHeader[outputTab.Data]] =
    solutionHeader.tupleRight( solutionHeader.groupCount ).traverse {
      case ( id, groupCount ) =>
        plans.ReadSolution.readPlanResult( planId, id, groupCount, outputTab )
    }

  override def getCustomGroupSelection( planId: PlanId ): ConnectionIO[Map[ClassName, Int]] =
    plans.CustomGroups.readCustomGroupSelection( planId )

  override def swapCustomGroupRowWithNext( planId: PlanId, group: Int, groupRow: Int ): ConnectionIO[Unit] =
    plans.CustomGroups.swapCustomGroupRowWithNext( planId, group, groupRow, up = false )

  override def swapCustomGroupRowWithPrevious( planId: PlanId, group: Int, groupRow: Int ): ConnectionIO[Unit] =
    plans.CustomGroups.swapCustomGroupRowWithNext( planId, group, groupRow - 1, up = true )

  override def toggleCustomGroupSectionBefore( planId: PlanId, group: Int, groupRow: Int ): ConnectionIO[Unit] =
    plans.CustomGroups.toggleCustomGroupSectionBefore( planId, group, groupRow )

  override def computePlan( planId: PlanId ): ConnectionIO[Unit] =
    plans.Headers
      .readPlanHeader( planId )
      .semiflatMap( header =>
        for {
          model        <- ReadModel.readModel( header.modelVersionId ) // TODO actually uses only extraction recipes
          solverInputs <- plans.ReadSolverInputs.getSolverInputs( planId )
          _ <- plans.WriteSolution
                 .writeSolution( planId, Calculator.computeFactory( model, solverInputs, ConstraintSolver ) )
        } yield ()
      )
      .getOrElse( () )

  override def recordTreeCommand( planId: PlanId, command: TreeCommand ): ConnectionIO[Unit] =
    PlanTrees.recordCommand( planId, command )
  override def resetTreeCommands( planId: PlanId ): ConnectionIO[Unit] =
    PlanTrees.resetCommands( planId )
}
