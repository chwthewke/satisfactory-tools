package net.chwthewke.satisfactorytools
package api

import cats.~>

import protocol.Page
import protocol.PageQuery
import protocol.PlanHeader
import protocol.PlanId
import protocol.UserId

trait LibraryApi[F[_]] { self =>
  def savePlan( userId: UserId, planId: PlanId, title: String ): F[PlanId]

  def copyPlan( userId: UserId, planId: PlanId ): F[PlanId]

  def getPlans( userId: UserId, page: Option[PageQuery] ): F[Page[PlanHeader]]

  def mapK[G[_]]( f: F ~> G ): LibraryApi[G] = new LibraryApi[G] {
    override def savePlan( userId: UserId, planId: PlanId, title: String ): G[PlanId] =
      f( self.savePlan( userId, planId, title ) )

    override def copyPlan( userId: UserId, planId: PlanId ): G[PlanId] =
      f( self.copyPlan( userId, planId ) )

    override def getPlans( userId: UserId, page: Option[PageQuery] ): G[Page[PlanHeader]] =
      f( self.getPlans( userId, page ) )
  }
}
