package net.chwthewke.satisfactorytools
package api

import cats.data.OptionT
import cats.~>

import model.Bill
import model.Options
import model.RecipeList
import model.ResourceOptions
import protocol.CustomGroupSelection
import protocol.InputTab
import protocol.OutputTab
import protocol.PlanHeader
import protocol.PlanId
import protocol.UserId

trait PlannerApi[F[_]] { self =>

  def newPlan( userId: UserId ): F[PlanId]

  def addCustomGroup( userId: UserId, planId: PlanId ): F[Boolean]

  def removeCustomGroup( userId: UserId, planId: PlanId ): F[Boolean]

  def setBill( userId: UserId, planId: PlanId, bill: Bill ): F[Unit]

  def setRecipeList( userId: UserId, planId: PlanId, recipeList: RecipeList ): F[Unit]

  def setOptions( userId: UserId, planId: PlanId, options: Options ): F[Unit]

  def setResourceOptions( userId: UserId, planId: PlanId, resourceOptions: ResourceOptions ): F[Unit]

  def setCustomGroupSelection( userId: UserId, planId: PlanId, selection: CustomGroupSelection ): F[Unit]

  def getPlanHeader( userId: UserId, planId: PlanId ): OptionT[F, PlanHeader]

  def getPlanQuery( userId: UserId, planId: PlanId, inputTab: InputTab ): OptionT[F, inputTab.Data]

  def getPlanResult( userId: UserId, planId: PlanId, outputTab: OutputTab ): OptionT[F, outputTab.Data]

  def computePlan( userId: UserId, planId: PlanId ): F[Unit]

  def mapK[G[_]]( f: F ~> G ): PlannerApi[G] = new PlannerApi[G] {
    override def newPlan( userId: UserId ): G[PlanId] = f( self.newPlan( userId ) )

    override def addCustomGroup( userId: UserId, planId: PlanId ): G[Boolean] =
      f( self.addCustomGroup( userId, planId ) )

    override def removeCustomGroup( userId: UserId, planId: PlanId ): G[Boolean] =
      f( self.removeCustomGroup( userId, planId ) )

    override def setBill( userId: UserId, planId: PlanId, bill: Bill ): G[Unit] =
      f( self.setBill( userId, planId, bill ) )

    override def setRecipeList( userId: UserId, planId: PlanId, recipeList: RecipeList ): G[Unit] =
      f( self.setRecipeList( userId, planId, recipeList ) )

    override def setOptions( userId: UserId, planId: PlanId, options: Options ): G[Unit] =
      f( self.setOptions( userId, planId, options ) )

    override def setResourceOptions( userId: UserId, planId: PlanId, resourceOptions: ResourceOptions ): G[Unit] =
      f( self.setResourceOptions( userId, planId, resourceOptions ) )

    override def setCustomGroupSelection( userId: UserId, planId: PlanId, selection: CustomGroupSelection ): G[Unit] =
      f( self.setCustomGroupSelection( userId, planId, selection ) )

    override def getPlanHeader( userId: UserId, planId: PlanId ): OptionT[G, PlanHeader] =
      self.getPlanHeader( userId, planId ).mapK( f )

    override def getPlanQuery( userId: UserId, planId: PlanId, inputTab: InputTab ): OptionT[G, inputTab.Data] =
      self.getPlanQuery( userId, planId, inputTab ).mapK( f )

    override def getPlanResult( userId: UserId, planId: PlanId, outputTab: OutputTab ): OptionT[G, outputTab.Data] =
      self.getPlanResult( userId, planId, outputTab ).mapK( f )

    override def computePlan( userId: UserId, planId: PlanId ): G[Unit] =
      f( self.computePlan( userId, planId ) )
  }

}
