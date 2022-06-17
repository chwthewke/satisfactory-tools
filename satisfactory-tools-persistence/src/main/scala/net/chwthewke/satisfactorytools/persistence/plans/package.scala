package net.chwthewke.satisfactorytools
package persistence

import doobie.ConnectionIO

import protocol.ModelVersionId
import protocol.PlanId

package object plans {
  private[plans] def readModelIds[A, B](
      planId: PlanId,
      f: ModelVersionId => ConnectionIO[Map[A, B]]
  ): ConnectionIO[Map[A, B]] =
    Headers
      .readPlanHeader( planId )
      .semiflatMap( header => f( header.modelVersionId ) )
      .getOrElse( Map.empty )

}
