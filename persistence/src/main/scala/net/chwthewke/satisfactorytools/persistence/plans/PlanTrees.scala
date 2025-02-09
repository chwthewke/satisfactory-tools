package net.chwthewke.satisfactorytools
package persistence
package plans

import cats.syntax.all._
import doobie._
import doobie.implicits._
import doobie.postgres.circe.jsonb.implicits._
import io.circe.Json
import io.circe.syntax._

import prod.tree.TreeCommand
import prod.tree.TreeCommands
import protocol.PlanId

object PlanTrees {
  def readPlanTreeCommands( planId: PlanId ): ConnectionIO[TreeCommands] =
    statements
      .selectPlanTreeCommands( planId )
      .option
      .flatMap( _.foldMapM( _.as[TreeCommands].liftTo[ConnectionIO] ) )

  def writePlanTreeCommands( planId: PlanId, commands: TreeCommands ): ConnectionIO[Unit] =
    statements.updatePlanTreeCommands( planId, commands.asJson ).run.void

  def recordCommand( planId: PlanId, command: TreeCommand ): ConnectionIO[Unit] =
    for {
      currentCommands <- readPlanTreeCommands( planId )
      newCommands = currentCommands.append( command )
      _ <- statements.updatePlanTreeCommands( planId, newCommands.asJson ).run.whenA( newCommands != currentCommands )
    } yield ()

  def resetCommands( planId: PlanId ): ConnectionIO[Unit] =
    statements.updatePlanTreeCommands( planId, TreeCommands.empty.asJson ).run.void

  object statements {
    def selectPlanTreeCommands( planId: PlanId ): Query0[Json] =
      // language=SQL
      sql"""SELECT "tree_commands"
           |FROM   "plans"
           |WHERE "id" = $planId
           |""".stripMargin //
        .query

    def updatePlanTreeCommands( planId: PlanId, commandsJson: Json ): Update0 =
      // language=SQL
      sql"""UPDATE "plans"
           |SET "tree_commands" = $commandsJson
           |WHERE "id" = $planId 
           |""".stripMargin //
        .update
  }
}
