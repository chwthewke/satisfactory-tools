package net.chwthewke.satisfactorytools
package persistence

import cats.data.OptionT
import cats.effect.Ref
import cats.effect.Sync
import cats.syntax.apply._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.semigroup._

import api.PlannerApi
import data.ClassName
import model.Bill
import model.Options
import model.RecipeList
import model.ResourceOptions
import prod.adv.tree.TreeCommand
import protocol.InputTab
import protocol.ModelVersionId
import protocol.OutputTab
import protocol.PlanHeader
import protocol.PlanId
import protocol.SolutionId
import protocol.UserId

class PlansWithTrees[F[_]: Sync]( store: Ref[F, Map[PlanId, Vector[TreeCommand]]], delegate: PlannerApi[F] )
    extends PlannerApi[F] {
  override def newPlan( userId: UserId, modelVersion: ModelVersionId ): F[PlanId] =
    delegate.newPlan( userId, modelVersion )

  override def addCustomGroup( planId: PlanId ): F[Boolean] = delegate.addCustomGroup( planId )

  override def removeCustomGroup( planId: PlanId ): F[Boolean] = delegate.removeCustomGroup( planId )

  override def setBill( planId: PlanId, bill: Bill ): F[Unit] = delegate.setBill( planId, bill )

  override def setRecipeList( planId: PlanId, recipeList: RecipeList ): F[Unit] =
    delegate.setRecipeList( planId, recipeList )

  override def addAllRecipes( planId: PlanId ): F[Unit] = delegate.addAllRecipes( planId )

  override def lockCurrentRecipes( planId: PlanId ): F[Unit] = delegate.lockCurrentRecipes( planId )

  override def addAllAlternatesToRecipeList( planId: PlanId ): F[Unit] = delegate.addAllAlternatesToRecipeList( planId )

  override def removeAllAlternatesFromRecipeList( planId: PlanId ): F[Unit] =
    delegate.removeAllAlternatesFromRecipeList( planId )

  override def setOptions( planId: PlanId, options: Options ): F[Unit] = delegate.setOptions( planId, options )

  override def setResourceOptions( planId: PlanId, resourceOptions: ResourceOptions ): F[Unit] =
    delegate.setResourceOptions( planId, resourceOptions )

  override def setCustomGroupSelection( planId: PlanId, groups: Map[ClassName, Int] ): F[Unit] =
    delegate.setCustomGroupSelection( planId, groups )

  override def setCustomGroupOrder( planId: PlanId, group: Int, groupRow: Int ): F[Unit] =
    delegate.setCustomGroupOrder( planId, group, groupRow )

  override def getPlanHeader( planId: PlanId ): OptionT[F, PlanHeader] = delegate.getPlanHeader( planId )

  override def getPlanQuery( planId: PlanId, inputTab: InputTab ): F[inputTab.Data] =
    delegate.getPlanQuery( planId, inputTab )

  override def getPlanResult( planId: PlanId, solutionId: SolutionId, outputTab: OutputTab ): F[outputTab.Data] =
    getPlanResultAux( planId, solutionId, outputTab )

  override def getCustomGroupSelection( planId: PlanId ): F[Map[ClassName, Int]] =
    delegate.getCustomGroupSelection( planId )

  override def computePlan( planId: PlanId ): F[Unit] = delegate.computePlan( planId )

  def recordCommand( planId: PlanId, command: TreeCommand ): F[Unit] =
    store.update( _ |+| Map( ( planId, Vector( command ) ) ) )

  private def getPlanResultAux[O]( planId: PlanId, solutionId: SolutionId, outputTab: OutputTab.Aux[O] ): F[O] =
    outputTab match {
      case OutputTab.Tree =>
        ( delegate.getPlanResult( planId, solutionId, OutputTab.Tree ), store.get.map( _.get( planId ) ) ).mapN(
          ( nakedTree, commandsOpt ) =>
            commandsOpt.orEmpty.foldLeft( nakedTree )( ( tree, cmd ) => tree.run( cmd ).getOrElse( tree ) )
        )
      case _ =>
        delegate.getPlanResult( planId, solutionId, outputTab )
    }
}
