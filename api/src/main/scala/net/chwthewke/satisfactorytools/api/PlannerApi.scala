package net.chwthewke.satisfactorytools
package api

import cats.data.OptionT
import cats.~>

import data.ClassName
import model.Bill
import model.Options
import model.RecipeList
import model.ResourceOptions
import prod.tree.TreeCommand
import protocol.InputTab
import protocol.ModelVersionId
import protocol.OutputTab
import protocol.PlanHeader
import protocol.PlanId
import protocol.SolutionHeader
import protocol.SolutionId
import protocol.UserId

trait PlannerApi[F[_]] { self =>

  def newPlan( userId: UserId, modelVersion: ModelVersionId ): F[PlanId]

  def addCustomGroup( planId: PlanId ): F[Boolean]

  def removeCustomGroup( planId: PlanId ): F[Boolean]

  def setBill( planId: PlanId, bill: Bill ): F[Unit]

  def setRecipeList( planId: PlanId, recipeList: RecipeList ): F[Unit]

  def addAllRecipes( planId: PlanId ): F[Unit]

  def lockCurrentRecipes( planId: PlanId ): F[Unit]

  def addAllAlternatesToRecipeList( planId: PlanId ): F[Unit]

  def removeAllAlternatesFromRecipeList( planId: PlanId ): F[Unit]

  def removeMatterConversionFromRecipeList( planId: PlanId ): F[Unit]

  def addRecipesUpToTier( planId: PlanId, tier: Int, alternates: Boolean ): F[Unit]

  def setOptions( planId: PlanId, options: Options ): F[Unit]

  def setResourceOptions( planId: PlanId, resourceOptions: ResourceOptions ): F[Unit]

  def setCustomGroupSelection( planId: PlanId, groups: Map[ClassName, Int] ): F[Unit]

  def swapCustomGroupRowWithNext( planId: PlanId, group: Int, groupRow: Int ): F[Unit]

  def swapCustomGroupRowWithPrevious( planId: PlanId, group: Int, groupRow: Int ): F[Unit]

  def toggleCustomGroupSectionBefore( planId: PlanId, group: Int, groupRow: Int ): F[Unit]

  def getPlanHeader( planId: PlanId ): OptionT[F, PlanHeader]

  def getPlanQuery( planId: PlanId, inputTab: InputTab ): F[inputTab.Data]

  def getPlanResult(
      planId: PlanId,
      solutionHeader: SolutionHeader[SolutionId],
      outputTab: OutputTab
  ): F[SolutionHeader[outputTab.Data]]

  def getCustomGroupSelection( planId: PlanId ): F[Map[ClassName, Int]]

  def computePlan( planId: PlanId ): F[Unit]

  def recordTreeCommand( planId: PlanId, command: TreeCommand ): F[Unit]

  def resetTreeCommands( planId: PlanId ): F[Unit]

  def mapK[G[_]]( f: F ~> G ): PlannerApi[G] = new PlannerApi[G] {

    override def newPlan( userId: UserId, modelVersion: ModelVersionId ): G[PlanId] =
      f( self.newPlan( userId, modelVersion ) )

    override def addCustomGroup( planId: PlanId ): G[Boolean] =
      f( self.addCustomGroup( planId ) )

    override def removeCustomGroup( planId: PlanId ): G[Boolean] =
      f( self.removeCustomGroup( planId ) )

    override def setBill( planId: PlanId, bill: Bill ): G[Unit] =
      f( self.setBill( planId, bill ) )

    override def setRecipeList( planId: PlanId, recipeList: RecipeList ): G[Unit] =
      f( self.setRecipeList( planId, recipeList ) )

    override def addAllRecipes( planId: PlanId ): G[Unit] =
      f( self.addAllRecipes( planId ) )

    override def lockCurrentRecipes( planId: PlanId ): G[Unit] =
      f( self.lockCurrentRecipes( planId ) )

    override def addAllAlternatesToRecipeList( planId: PlanId ): G[Unit] =
      f( self.addAllAlternatesToRecipeList( planId ) )

    override def removeAllAlternatesFromRecipeList( planId: PlanId ): G[Unit] =
      f( self.removeAllAlternatesFromRecipeList( planId ) )

    override def removeMatterConversionFromRecipeList( planId: PlanId ): G[Unit] =
      f( self.removeMatterConversionFromRecipeList( planId ) )

    override def addRecipesUpToTier( planId: PlanId, tier: Int, alternates: Boolean ): G[Unit] =
      f( self.addRecipesUpToTier( planId, tier, alternates ) )

    override def setOptions( planId: PlanId, options: Options ): G[Unit] =
      f( self.setOptions( planId, options ) )

    override def setResourceOptions( planId: PlanId, resourceOptions: ResourceOptions ): G[Unit] =
      f( self.setResourceOptions( planId, resourceOptions ) )

    override def setCustomGroupSelection( planId: PlanId, groups: Map[ClassName, Int] ): G[Unit] =
      f( self.setCustomGroupSelection( planId, groups ) )

    override def swapCustomGroupRowWithNext( planId: PlanId, group: Int, groupRow: Int ): G[Unit] =
      f( self.swapCustomGroupRowWithNext( planId, group, groupRow ) )

    override def swapCustomGroupRowWithPrevious( planId: PlanId, group: Int, groupRow: Int ): G[Unit] =
      f( self.swapCustomGroupRowWithPrevious( planId, group, groupRow ) )

    override def toggleCustomGroupSectionBefore( planId: PlanId, group: Int, groupRow: Int ): G[Unit] =
      f( self.toggleCustomGroupSectionBefore( planId, group, groupRow ) )

    override def getPlanHeader( planId: PlanId ): OptionT[G, PlanHeader] =
      self.getPlanHeader( planId ).mapK( f )

    override def getPlanQuery( planId: PlanId, inputTab: InputTab ): G[inputTab.Data] =
      f( self.getPlanQuery( planId, inputTab ) )

    override def getPlanResult(
        planId: PlanId,
        solutionHeader: SolutionHeader[SolutionId],
        outputTab: OutputTab
    ): G[SolutionHeader[outputTab.Data]] =
      f( self.getPlanResult( planId, solutionHeader, outputTab ) )

    override def getCustomGroupSelection( planId: PlanId ): G[Map[ClassName, Int]] =
      f( self.getCustomGroupSelection( planId ) )

    override def computePlan( planId: PlanId ): G[Unit] =
      f( self.computePlan( planId ) )

    override def recordTreeCommand( planId: PlanId, command: TreeCommand ): G[Unit] =
      f( self.recordTreeCommand( planId, command ) )

    override def resetTreeCommands( planId: PlanId ): G[Unit] =
      f( self.resetTreeCommands( planId ) )
  }

}
