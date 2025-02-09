package net.chwthewke.satisfactorytools
package persistence

import cats.data.OptionT
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.show._
import doobie._
import doobie.implicits._
import doobie.postgres.implicits._
import doobie.util.invariant.UnexpectedEnd

import api.LibraryApi
import persistence.plans.PlanTrees
import protocol.InputTab
import protocol.ModelVersionId
import protocol.Page
import protocol.PageQuery
import protocol.PlanHeader
import protocol.PlanId
import protocol.PlanName
import protocol.SolutionId
import protocol.UserId

object Library extends LibraryApi[ConnectionIO] {

  override def savePlan( header: PlanHeader, title: PlanName ): ConnectionIO[PlanId] =
    (
      OptionT.when[ConnectionIO, Unit]( header.title.forall( _ == title ) )( () ) *>
        OptionT
          .fromOption[ConnectionIO]( header.srcId )
          .semiflatTap( srcId => copyPlanParts( header.id, srcId ) )
          .orElse( OptionT.when[ConnectionIO, PlanId]( !header.isTransient )( header.id ) )
    ).getOrElseF( makePlanCopy( header, None, Some( title ) ) ) <*
      deletePlan( header.owner, header.id ).whenA( header.isTransient )

  override def editPlan( header: PlanHeader, hasChanges: Boolean ): ConnectionIO[PlanId] =
    if (header.isTransient || !hasChanges)
      header.id.pure[ConnectionIO]
    else
      makePlanCopy( header, Some( header.id ), None )

  private def makePlanCopy( header: PlanHeader, srcId: Option[PlanId], name: Option[PlanName] ): ConnectionIO[PlanId] =
    createPlan( header.owner, header.modelVersionId, srcId, name )
      .flatTap( copyPlanParts( header.id, _ ) )

  override def deletePlan( userId: UserId, planId: PlanId ): ConnectionIO[Unit] =
    statements.deletePlan.run( planId ).void

  override def migratePlan( userId: UserId, planId: PlanId ): ConnectionIO[PlanId] =
    for {
      header <- Plans
                  .getPlanHeader( planId )
                  .getOrElseF( FC.raiseError( new IllegalArgumentException( show"Unknown plan #$planId" ) ) )
      versions               <- ReadModel.getModelVersions
      ( versionId, version ) <- versions.lastOption.liftTo[ConnectionIO]( Error( "No model version to migrate to" ) )
      newId                  <- createPlan( userId, versionId, None, header.title.map( _.migrated( version ) ) )
      _                      <- copyPlanInputParts( planId, newId, Some( versionId ) )
      _                      <- plans.WriteSolution.clearSolution( newId )
    } yield newId

  override def getAllPlans( userId: UserId ): ConnectionIO[Vector[PlanHeader]] =
    statements.selectPlanHeaders( userId ).to[Vector]

  override def getPlans( userId: UserId, page: PageQuery ): ConnectionIO[Page[PlanHeader]] =
    statements
      .selectPlanHeadersPage( userId, page.limit + 1L, page.offset )
      .to[Vector]
      .map { plans =>
        Page(
          plans.take( page.pageSize ),
          page,
          hasPreviousPage = page.pageNum > 1,
          hasNextPage = plans.size > page.pageSize
        )
      }

  private def createPlan(
      userId: UserId,
      modelVersionId: ModelVersionId,
      srcId: Option[PlanId],
      name: Option[PlanName]
  ): ConnectionIO[PlanId] =
    statements.insertPlan
      .withUniqueGeneratedKeys[PlanId]( "id" )( ( userId, modelVersionId, srcId, name ) )
      .adaptErr {
        case UnexpectedEnd =>
          Error( s"Unable to createPlan $name from $srcId for user #$userId, model $modelVersionId" )
      }

  private def copyPlanParts( from: PlanId, to: PlanId ): ConnectionIO[Unit] =
    for {
      _ <- copyPlanInputParts( from, to, None )
      _ <- plans.WriteSolution.clearSolution( to )
      _ <- copySolution( from, to )
      _ <- copyPlanTree( from, to )
    } yield ()

  private def copyPlanInputParts(
      from: PlanId,
      to: PlanId,
      migrateResourceOptions: Option[ModelVersionId]
  ): ConnectionIO[Unit] =
    for {
      // TODO optimized SQL for the inputs part?
      bill       <- Plans.getPlanQuery( from, InputTab.Bill )
      _          <- Plans.setBill( to, bill )
      recipeList <- Plans.getPlanQuery( from, InputTab.Recipes )
      _          <- Plans.setRecipeList( to, recipeList )
      options    <- Plans.getPlanQuery( from, InputTab.Options )
      _          <- Plans.setOptions( to, options )
      resources  <- Plans.getPlanQuery( from, InputTab.ResourceOptions )
      migratedResources <-
        OptionT
          .fromOption[ConnectionIO]( migrateResourceOptions )
          .semiflatMap( version => ReadModel.readDefaultResourceOptions( version ) )
          .map( defaultResourceOptions => resources.mergeResourceNodes( defaultResourceOptions.resourceNodes ) )
          .getOrElse( resources )
      _ <- Plans.setResourceOptions( to, migratedResources )
    } yield ()

  private def copySolution( from: PlanId, to: PlanId ): ConnectionIO[Unit] =
    statements.copyPlanSolution
      .withGeneratedKeys[SolutionId]( "id" )( ( to, from ) )
      .evalTap( copySolutionItems( from, _ ) )
      .compile
      .drain

  private def copyPlanTree( from: PlanId, to: PlanId ): ConnectionIO[Unit] =
    PlanTrees.readPlanTreeCommands( from ).flatMap( PlanTrees.writePlanTreeCommands( to, _ ) )

  private def copySolutionItems( from: PlanId, to: SolutionId ): ConnectionIO[Unit] =
    for {
      _ <- statements.copySolutionExtractionRecipes.run( ( to, from ) )
      _ <- statements.copySolutionManufacturingRecipes.run( ( to, from ) )
      _ <- statements.copySolutionExtraInputs.run( ( to, from ) )
      _ <- statements.copySolutionExtraOutputs.run( ( to, from ) )
    } yield ()

  private[persistence] object statements {

    val insertPlan: Update[( UserId, ModelVersionId, Option[PlanId], Option[PlanName] )] =
      Update(
        // language=SQL
        """INSERT INTO "plans"
          |  ( "user_id"
          |  , "model_version_id"
          |  , "src_id"
          |  , "name"
          |  )
          |VALUES
          |  ( ?, ?, ?, ? )
          |""".stripMargin
      )

    private def selectPlanHeadersStmt( userId: UserId ): Fragment =
      // language=SQL
      sql"""SELECT
           |    p."id"
           |  , p."model_version_id"
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
           |WHERE p."user_id" = $userId
           |GROUP BY p."id", p."updated", q."name", s."id"
           |ORDER BY p."updated" DESC
           |""".stripMargin

    def selectPlanHeaders( userId: UserId ): Query0[PlanHeader] =
      selectPlanHeadersStmt( userId ).query[PlanHeader.Row].map( PlanHeader.apply )

    def selectPlanHeadersPage( userId: UserId, count: Long, offset: Long ): Query0[PlanHeader] =
      sql"""${selectPlanHeadersStmt( userId )}
           |LIMIT $count OFFSET $offset
           |""".stripMargin //
        .query[PlanHeader.Row]
        .map( PlanHeader.apply )

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
          |  , "group_order"
          |  , "section_before"
          |  )
          |  SELECT
          |    ?
          |  , "recipe_id"
          |  , "amount"
          |  , "custom_group"
          |  , "group_order"
          |  , "section_before"
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
