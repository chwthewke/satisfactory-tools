package net.chwthewke.satisfactorytools
package persistence
package plans

import cats.data.OptionT
import doobie._
import doobie.postgres.implicits._

import model.Options
import model.ResourceOptions
import protocol.PlanHeader
import protocol.PlanId
import protocol.UserId

object Headers {
  def writeNewPlan(
      userId: UserId,
      options: Options,
      resourceOptions: ResourceOptions
  ): ConnectionIO[PlanId] =
    for {
      planId  <- statements.insertUnnamedPlan.withUniqueGeneratedKeys[PlanId]( "id" )( userId )
      itemIds <- ReadModel.readItemIds
      _       <- WriteSolverInputs.updateOptions( planId, options )
      _       <- WriteSolverInputs.updateResourceOptions( planId, itemIds, resourceOptions )
    } yield planId

  def readPlanHeader( planId: PlanId ): OptionT[ConnectionIO, PlanHeader] =
    OptionT( statements.selectPlanHeader.toQuery0( planId ).option )

  private[plans] object statements {

    val insertUnnamedPlan: Update[UserId] =
      Update(
        // language=SQL
        """INSERT INTO "plans"
          |  ( "user_id" )
          |VALUES
          |  ( ? )
          |""".stripMargin
      )

    val selectPlanHeader: Query[PlanId, PlanHeader] =
      Query[PlanId, PlanHeader.Row](
        // language=SQL
        """SELECT
          |    p."id"
          |  , p."user_id"
          |  , p."name"
          |  , q."name"
          |  , p."src_id"
          |  , p."updated"
          |  , s."id"
          |  , s."error_message"
          |  , s."custom_groups"
          |  , MAX( r."custom_group" )
          |FROM        "plans"                          p
          |LEFT JOIN   "plans"                          q ON q."id" = p."src_id"
          |LEFT JOIN   "plan_solutions"                 s ON p."id" = s."plan_id"
          |LEFT JOIN   "solution_manufacturing_recipes" r ON s."id" = r."solution_id"
          |WHERE p."id" = ?
          |GROUP BY p."id", q."name", s."id"
          |""".stripMargin
      ).map( PlanHeader.apply )

  }
}
