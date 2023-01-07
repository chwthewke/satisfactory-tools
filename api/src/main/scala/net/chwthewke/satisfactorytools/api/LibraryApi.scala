package net.chwthewke.satisfactorytools
package api

import cats.~>

import protocol.Page
import protocol.PageQuery
import protocol.PlanHeader
import protocol.PlanId
import protocol.PlanName
import protocol.UserId

trait LibraryApi[F[_]] { self =>
  def savePlan( userId: UserId, planId: PlanId, srcIdOpt: Option[PlanId], title: PlanName ): F[PlanId]

  def editPlan( userId: UserId, planId: PlanId ): F[PlanId]

  def copyPlan( userId: UserId, planId: PlanId ): F[PlanId]

  def deletePlan( userId: UserId, planId: PlanId ): F[Unit]

  def migratePlan( userId: UserId, planId: PlanId ): F[PlanId]

  def getAllPlans( userId: UserId ): F[Vector[PlanHeader]]

  def getPlans( userId: UserId, page: PageQuery ): F[Page[PlanHeader]]

  def mapK[G[_]]( f: F ~> G ): LibraryApi[G] = new LibraryApi[G] {
    override def savePlan( userId: UserId, planId: PlanId, srcIdOpt: Option[PlanId], title: PlanName ): G[PlanId] =
      f( self.savePlan( userId, planId, srcIdOpt, title ) )

    override def editPlan( userId: UserId, planId: PlanId ): G[PlanId] =
      f( self.editPlan( userId, planId ) )

    override def copyPlan( userId: UserId, planId: PlanId ): G[PlanId] =
      f( self.copyPlan( userId, planId ) )

    override def deletePlan( userId: UserId, planId: PlanId ): G[Unit] =
      f( self.deletePlan( userId, planId ) )

    override def migratePlan( userId: UserId, planId: PlanId ): G[PlanId] =
      f( self.migratePlan( userId, planId ) )

    override def getAllPlans( userId: UserId ): G[Vector[PlanHeader]] =
      f( self.getAllPlans( userId ) )

    override def getPlans( userId: UserId, page: PageQuery ): G[Page[PlanHeader]] =
      f( self.getPlans( userId, page ) )
  }
}
