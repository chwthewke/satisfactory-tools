package net.chwthewke.satisfactorytools
package persistence

import cats.data.OptionT
import cats.syntax.functor._
import cats.syntax.show._
import doobie._
import doobie.postgres.implicits._
import java.time.Instant

import api.LibraryApi
import protocol.InputTab
import protocol.Page
import protocol.PageQuery
import protocol.PlanHeader
import protocol.PlanId
import protocol.PlanName
import protocol.SolutionId
import protocol.UserId

object Library extends LibraryApi[ConnectionIO] {

  override def savePlan(
      userId: UserId,
      planId: PlanId,
      srcIdOpt: Option[PlanId],
      title: PlanName
  ): ConnectionIO[PlanId] =
    OptionT( statements.selectPlanByName.option( ( userId, title ) ) )
      .orElse( OptionT.fromOption( srcIdOpt ) )
      .foldF(
        statements.updatePlanName.run( ( title, planId ) ).as( planId )
      )(
        targetId =>
          for {
            _ <- statements.updatePlanName.run( ( title, targetId ) )
            _ <- copyPlanParts( planId, targetId )
            _ <- statements.deletePlan.run( planId )
          } yield targetId
      )

  override def editPlan( userId: UserId, planId: PlanId ): ConnectionIO[PlanId] =
    for {
      newId <- createPlan( userId, Some( planId ), None )
      _     <- copyPlanParts( planId, newId )
    } yield newId

  override def copyPlan( userId: UserId, planId: PlanId ): ConnectionIO[PlanId] =
    for {
      header <- Plans
                 .getPlanHeader( planId )
                 .getOrElseF( FC.raiseError( new IllegalArgumentException( show"Unknown plan #$planId" ) ) )
      newId <- createPlan( userId, None, header.title.map( _.copy ) )
      _     <- copyPlanParts( planId, newId )
    } yield newId

  override def deletePlan( userId: UserId, planId: PlanId ): ConnectionIO[Unit] =
    statements.deletePlan.run( planId ).void

  override def getAllPlans( userId: UserId ): ConnectionIO[Vector[PlanHeader]] =
    statements.selectPlanHeaders.toQuery0( userId ).to[Vector]

  override def getPlans( userId: UserId, page: PageQuery ): ConnectionIO[Page[PlanHeader]] =
    statements.selectPlanHeadersPage
      .toQuery0( ( userId, page.limit + 1L, page.offset ) )
      .to[Vector]
      .map { plans =>
        Page(
          plans.take( page.pageSize ),
          page,
          hasPreviousPage = page.pageNum > 1,
          hasNextPage = plans.size > page.pageSize
        )
      }

  private def createPlan( userId: UserId, srcId: Option[PlanId], name: Option[PlanName] ): ConnectionIO[PlanId] =
    statements.insertPlan.withUniqueGeneratedKeys[PlanId]( "id" )( ( userId, srcId, name ) )

  private def copyPlanParts( from: PlanId, to: PlanId ): ConnectionIO[Unit] =
    for {
      // TODO optimized SQL for the inputs part?
      bill       <- Plans.getPlanQuery( from, InputTab.Bill )
      _          <- Plans.setBill( to, bill )
      recipeList <- Plans.getPlanQuery( from, InputTab.Recipes )
      _          <- Plans.setRecipeList( to, recipeList )
      options    <- Plans.getPlanQuery( from, InputTab.Options )
      _          <- Plans.setOptions( to, options )
      resources  <- Plans.getPlanQuery( from, InputTab.ResourceOptions )
      _          <- Plans.setResourceOptions( to, resources )
      //
      _ <- plans.WriteSolution.clearSolution( to )
      _ <- copySolution( from, to )
    } yield ()

  private def copySolution( from: PlanId, to: PlanId ): ConnectionIO[Unit] =
    for {
      newId <- statements.copyPlanSolution.withUniqueGeneratedKeys[SolutionId]( "id" )( ( to, from ) )
      _     <- copySolutionItems( from, newId )
    } yield ()

  private def copySolutionItems( from: PlanId, to: SolutionId ): ConnectionIO[Unit] =
    for {
      _ <- statements.copySolutionExtractionRecipes.run( ( to, from ) )
      _ <- statements.copySolutionManufacturingRecipes.run( ( to, from ) )
      _ <- statements.copySolutionExtraInputs.run( ( to, from ) )
      _ <- statements.copySolutionExtraOutputs.run( ( to, from ) )
    } yield ()

  private[persistence] object statements {

    val insertPlan: Update[( UserId, Option[PlanId], Option[PlanName] )] =
      Update(
        // language=SQL
        """INSERT INTO "plans"
          |  ( "user_id"
          |  , "src_id"
          |  , "name"
          |  )
          |VALUES
          |  ( ?, ?, ? )
          |""".stripMargin
      )

    // TODO weird near-dupe, so maybe reorganize?
    type PlanHeaderRow =
      (
          PlanId,
          UserId,
          Option[PlanName],
          Option[PlanName],
          Option[PlanId],
          Instant,
          Option[SolutionId],
          Option[String],
          Option[Int],
          Option[Int]
      )

    private val fromPlanHeaderRow: PlanHeaderRow => PlanHeader = {
      case (
          id,
          owner,
          nameOpt,
          srcNameOpt,
          srcIdOpt,
          updated,
          solutionIdOpt,
          solutionErrorOpt,
          groupCountOpt,
          lastGroupOpt
          ) =>
        PlanHeader(
          id,
          owner,
          nameOpt,
          srcNameOpt,
          srcIdOpt,
          updated,
          solutionIdOpt,
          solutionErrorOpt,
          groupCountOpt,
          lastGroupOpt
        )
    }

    private val selectPlanHeadersStmt: String =
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
        |WHERE p."user_id" = ?
        |GROUP BY p."id", p."updated", q."name", s."id"
        |ORDER BY p."updated" DESC
        |""".stripMargin

    val selectPlanHeaders: Query[UserId, PlanHeader] =
      Query[UserId, PlanHeaderRow]( selectPlanHeadersStmt ).map( fromPlanHeaderRow )

    val selectPlanHeadersPage: Query[( UserId, Long, Long ), PlanHeader] =
      Query[( UserId, Long, Long ), PlanHeaderRow](
        selectPlanHeadersStmt ++
          """LIMIT ? OFFSET ?
            |""".stripMargin
      ).map( fromPlanHeaderRow )

    val copyPlanSolution: Update[( PlanId, PlanId )] =
      Update(
        // language=SQL
        """INSERT INTO "plan_solutions" 
          |  ( "plan_id"
          |  , "custom_groups"
          |  , "error_message"
          |  )
          |  SELECT 
          |    ?
          |  , "custom_groups"
          |  , "error_message"
          |  FROM "plan_solutions"
          |  WHERE "plan_id" = ?
          |""".stripMargin
      )

    val copySolutionExtractionRecipes: Update[( SolutionId, PlanId )] =
      Update(
        // language=SQL
        """INSERT INTO "solution_extraction_recipes"
          |  ( "solution_id"
          |  , "recipe_id"
          |  , "amount"
          |  , "clock_speed"
          |  )
          |  SELECT
          |    ?
          |  , "recipe_id"
          |  , "amount"
          |  , "clock_speed"
          |  FROM       "solution_extraction_recipes" r
          |  INNER JOIN "plan_solutions"              s ON r."solution_id" = s."id"
          |  WHERE s."plan_id" = ?
          |""".stripMargin
      )

    val copySolutionManufacturingRecipes: Update[( SolutionId, PlanId )] =
      Update(
        // language=SQL
        """INSERT INTO "solution_manufacturing_recipes"
          |  ( "solution_id"
          |  , "recipe_id"
          |  , "amount"
          |  , "custom_group"
          |  )
          |  SELECT
          |    ?
          |  , "recipe_id"
          |  , "amount"
          |  , "custom_group"
          |  FROM       "solution_manufacturing_recipes" r
          |  INNER JOIN "plan_solutions" s ON r."solution_id" = s."id"
          |  WHERE s."plan_id" = ?
          |""".stripMargin
      )

    val copySolutionExtraInputs: Update[( SolutionId, PlanId )] =
      Update(
        // language=SQL
        """INSERT INTO "solution_extra_inputs" 
          |  ( "solution_id"
          |  , "item_id"
          |  , "amount"
          |  )
          |  SELECT
          |    ?
          |  , "item_id"
          |  , "amount"
          |  FROM       "solution_extra_inputs" x
          |  INNER JOIN "plan_solutions"        s ON s."id" = x."solution_id"
          |  WHERE s."plan_id" = ?
          |""".stripMargin
      )

    val copySolutionExtraOutputs: Update[( SolutionId, PlanId )] =
      Update(
        // language=SQL
        """INSERT INTO "solution_extra_outputs" 
          |  ( "solution_id"
          |  , "item_id"
          |  , "amount"
          |  )
          |  SELECT
          |    ?
          |  , "item_id"
          |  , "amount"
          |  FROM       "solution_extra_outputs" x
          |  INNER JOIN "plan_solutions"         s ON s."id" = x."solution_id"
          |  WHERE s."plan_id" = ?
          |""".stripMargin
      )

    val selectPlanByName: Query[( UserId, PlanName ), PlanId] =
      Query(
        // language=SQL
        """SELECT "id" FROM "plans" 
          |WHERE "user_id" = ?
          |  AND "name"    = ?
          |""".stripMargin
      )

    val updatePlanName: Update[( PlanName, PlanId )] =
      Update(
        // language=SQL
        """UPDATE "plans"
          |SET "name" = ?
          |WHERE "id" = ?
          |""".stripMargin
      )

    val deletePlan: Update[PlanId] =
      Update(
        /// language=SQL
        """DELETE FROM "plans"
          |WHERE "id" = ?
          |""".stripMargin
      )

  }
}
