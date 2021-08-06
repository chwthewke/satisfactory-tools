package net.chwthewke.satisfactorytools
package api

import cats.data.OptionT
import cats.~>

import data.ClassName
import model.Bill
import model.Options
import model.RecipeList
import model.ResourceOptions
import protocol.InputTab
import protocol.OutputTab
import protocol.PlanHeader
import protocol.PlanId
import protocol.SolutionId
import protocol.UserId

trait PlannerApi[F[_]] { self =>

  def newPlan( userId: UserId, options: Options, resourceOptions: ResourceOptions ): F[PlanId]

  def addCustomGroup( planId: PlanId ): F[Boolean]

  def removeCustomGroup( planId: PlanId ): F[Boolean]

  def setBill( planId: PlanId, bill: Bill ): F[Unit]

  def setRecipeList( planId: PlanId, recipeList: RecipeList ): F[Unit]

  def setOptions( planId: PlanId, options: Options ): F[Unit]

  def setResourceOptions( planId: PlanId, resourceOptions: ResourceOptions ): F[Unit]

  def setCustomGroupSelection( planId: PlanId, groups: Map[ClassName, Int] ): F[Unit]

  def setCustomGroupOrder( planId: PlanId, group: Int, groupRow: Int ): F[Unit]

  def getPlanHeader( planId: PlanId ): OptionT[F, PlanHeader]

  def getPlanQuery( planId: PlanId, inputTab: InputTab ): F[inputTab.Data]

  def getPlanResult( planId: PlanId, solutionId: SolutionId, outputTab: OutputTab ): F[outputTab.Data]

  def getCustomGroupSelection( planId: PlanId ): F[Map[ClassName, Int]]

  def computePlan( planId: PlanId ): F[Unit]

  def mapK[G[_]]( f: F ~> G ): PlannerApi[G] = new PlannerApi[G] {

    override def newPlan( userId: UserId, options: Options, resourceOptions: ResourceOptions ): G[PlanId] =
      f( self.newPlan( userId, options, resourceOptions ) )

    override def addCustomGroup( planId: PlanId ): G[Boolean] =
      f( self.addCustomGroup( planId ) )

    override def removeCustomGroup( planId: PlanId ): G[Boolean] =
      f( self.removeCustomGroup( planId ) )

    override def setBill( planId: PlanId, bill: Bill ): G[Unit] =
      f( self.setBill( planId, bill ) )

    override def setRecipeList( planId: PlanId, recipeList: RecipeList ): G[Unit] =
      f( self.setRecipeList( planId, recipeList ) )

    override def setOptions( planId: PlanId, options: Options ): G[Unit] =
      f( self.setOptions( planId, options ) )

    override def setResourceOptions( planId: PlanId, resourceOptions: ResourceOptions ): G[Unit] =
      f( self.setResourceOptions( planId, resourceOptions ) )

    override def setCustomGroupSelection( planId: PlanId, groups: Map[ClassName, Int] ): G[Unit] =
      f( self.setCustomGroupSelection( planId, groups ) )

    override def setCustomGroupOrder( planId: PlanId, group: Int, groupRow: Int ): G[Unit] =
      f( self.setCustomGroupOrder( planId, group, groupRow ) )

    override def getPlanHeader( planId: PlanId ): OptionT[G, PlanHeader] =
      self.getPlanHeader( planId ).mapK( f )

    override def getPlanQuery( planId: PlanId, inputTab: InputTab ): G[inputTab.Data] =
      f( self.getPlanQuery( planId, inputTab ) )

    override def getPlanResult( planId: PlanId, solutionId: SolutionId, outputTab: OutputTab ): G[outputTab.Data] =
      f( self.getPlanResult( planId, solutionId, outputTab ) )

    override def getCustomGroupSelection( planId: PlanId ): G[Map[ClassName, Int]] =
      f( self.getCustomGroupSelection( planId ) )

    override def computePlan( planId: PlanId ): G[Unit] =
      f( self.computePlan( planId ) )
  }

}
