package net.chwthewke.satisfactorytools
package persistence
package plans

import cats.data.NonEmptyVector
import cats.data.OptionT
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.functor._
import cats.syntax.functorFilter._
import doobie._
import doobie.implicits._

import data.ClassName
import data.Countable
import model.GroupAssignments
import protocol.PlanHeader
import protocol.PlanId
import protocol.RecipeId
import protocol.SolutionHeader
import protocol.SolutionId

object CustomGroups {

  def readCustomGroupSelection( planId: PlanId ): ConnectionIO[Map[ClassName, Int]] =
    Headers
      .readPlanHeader( planId )
      .semiflatMap( header =>
        (
          readPlanCustomGroups( header ),
          ReadModel.readRecipes( header.modelVersionId )
        ).mapN {
          case ( groups, recipes ) =>
            groups.groupsByItem
              .flatMap {
                case ( recipeId, ix ) => recipes.get( recipeId ).map( _.className ).tupleRight( ix )
              }
        }
      )
      .getOrElse( Map.empty )

  private[plans] def readPlanCustomGroups( header: PlanHeader ): ConnectionIO[GroupAssignments[RecipeId]] =
    OptionT
      .fromOption[ConnectionIO]( header.solution.valueAndCount )
      .semiflatMap { case ( solutionId, groupCount ) => readCustomGroups( solutionId, groupCount ) }
      .getOrElse( GroupAssignments.emptyGroups( SolutionHeader.DefaultCustomGroups ) )

  type GroupOrderRow  = ( SolutionId, RecipeId, Int, Boolean )
  type GroupOrderSwap = ( GroupOrderRow, GroupOrderRow, Option[GroupOrderRow] )

  private def getCustomGroupRows(
      planId: PlanId,
      group: Int,
      from: Int,
      thirdRow: Boolean
  ): OptionT[ConnectionIO, GroupOrderSwap] = {
    val queriedRows: NonEmptyVector[Int] =
      if (thirdRow) NonEmptyVector.of( 0, 1, 2 ) else NonEmptyVector.of( 0, 1 )

//    import cats.syntax.flatMap._
    OptionT(
      statements
        .selectGroupOrder( planId, group, queriedRows.map( from + _ ) )
        .stream
        .mapFilter { case ( solId, recipeId, orderOpt, section ) => orderOpt.map( ( solId, recipeId, _, section ) ) }
        .compile
        .toVector
        // .flatTap( rows => FC.delay( println( rows.mkString( "READ ROWS\n  ", "\n  ", "" ) ) ) )
        .map( vec => ( vec.headOption, vec.lift( 1 ) ).mapN( ( _, _, vec.lift( 2 ) ) ) )
    )
  }

  private def swapDown( rows: GroupOrderSwap ): Vector[GroupOrderRow] = rows match {
    case (
          ( sol1, rec1, ord1, sec1 ),
          ( sol2, rec2, ord2, sec2 ),
          _
        ) if sol2 == sol1 && ord2 == ord1 + 1 =>
      if (sec2)
        Vector(
          ( sol1, rec1, ord1, true ),
          ( sol2, rec2, ord2, false )
        )
      else
        Vector(
          ( sol2, rec2, ord1, sec1 ),
          ( sol1, rec1, ord2, false )
        )

    case _ => Vector.empty
  }

  private def swapUp( rows: GroupOrderSwap ): Vector[GroupOrderRow] = rows match {
    case (
          r1 @ ( sol1, rec1, ord1, sec1 ),
          ( sol2, rec2, ord2, sec2 ),
          next
        )
        if sol2 == sol1 && ord2 == ord1 + 1
          && next.forall { case ( sol3, _, ord3, _ ) => sol3 == sol1 && ord3 == ord2 + 1 } =>
      if (sec2)
        Vector( r1, ( sol2, rec2, ord2, false ) ) ++ next.map( _.copy( _4 = true ) )
      else
        Vector(
          ( sol2, rec2, ord1, sec1 ),
          ( sol1, rec1, ord2, false )
        )

    case _ => Vector.empty
  }

  def swapCustomGroupRowWithNext( planId: PlanId, group: Int, groupRow: Int, up: Boolean ): ConnectionIO[Unit] =
    getCustomGroupRows( planId, group, groupRow, up )
      .foreachF { rows =>
        val swapped: Vector[( SolutionId, RecipeId, Int, Boolean )] = if (up) swapUp( rows ) else swapDown( rows )
//        FC.delay( println( swapped.mkString( "WRITTEN ROWS\n  ", "\n  ", "" ) ) ) *>
        statements.updateGroupOrder( group ).updateMany( swapped ).void
      }

  def toggleCustomGroupSectionBefore( planId: PlanId, group: Int, groupRow: Int ): ConnectionIO[Unit] =
    statements.toggleSectionBefore( planId, group, groupRow ).run.whenA( groupRow > 0 )

  def readCustomGroups( solutionId: SolutionId, groupCount: Int ): ConnectionIO[GroupAssignments[RecipeId]] =
    statements.selectRecipeGroups
      .toQuery0( solutionId )
      .stream
      .compile
      .fold( GroupAssignments.emptyGroups[RecipeId]( groupCount ) ) {
        case ( groups, ( recipe, groupOpt, sectionBefore ) ) =>
          groupOpt.foldLeft( groups )( _.append( _, recipe, sectionBefore ) )
      }

  def updateCustomGroupSelection(
      planId: PlanId,
      groups: Map[ClassName, Int]
  ): ConnectionIO[Unit] =
    Headers
      .readPlanHeader( planId )
      .subflatMap( h => h.solution.valueAndCount.tupleLeft( h.modelVersionId ) )
      .semiflatMap {
        case ( modelVersion, ( solutionId, groupCount ) ) =>
          for {
            recipeIds <- ReadModel.readRecipeIds( modelVersion )
            assignments = groups.flatMap { case ( cn, ix ) => recipeIds.get( cn ).tupleRight( ix ) }
            // TODO reading same table twice (with different ORDER BY clauses, but not using order in 2nd case)...
            // TODO also an opportunity to avoid the read/diff/write from Application
            currentGroups <- readCustomGroups( solutionId, groupCount )
            recipes       <- statements.selectUngroupedManufacturingRecipes.toQuery0( solutionId ).to[Vector]

            _ <- WriteSolution.writeManufacturingRecipesIds(
                   solutionId,
                   currentGroups.update(
                     recipes.mapFilter( cr => assignments.get( cr.item ).tupleLeft( cr.item ) ).toMap
                   ),
                   recipes
                 )
          } yield ()
      }
      .value
      .void

  def updateGroupCountIncrement( planId: PlanId ): ConnectionIO[Boolean] =
    Headers
      .readPlanHeader( planId )
      .map( _.solution )
      .filter( _.canAddGroup )
      .subflatMap( _.valueAndCount )
      .semiflatMap {
        case ( solutionId, groupCount ) =>
          statements.updateGroupCount.run( ( groupCount + 1, solutionId ) )
      }
      .fold( false )( _ > 0 )

  def updateGroupCountDecrement( planId: PlanId ): ConnectionIO[Boolean] =
    Headers
      .readPlanHeader( planId )
      .map( _.solution )
      .filter( _.canRemoveGroup )
      .subflatMap( _.valueAndCount )
      .semiflatMap {
        case ( solutionId, groupCount ) =>
          statements.updateGroupCount.run( ( groupCount - 1, solutionId ) )
      }
      .fold( false )( _ > 0 )

  private[plans] object statements {

    val selectRecipeGroups: Query[SolutionId, ( RecipeId, Option[Int], Boolean )] =
      Query(
        // language=SQL
        """SELECT
          |    "recipe_id"
          |  , "custom_group"
          |  , "section_before"
          |FROM "solution_manufacturing_recipes"
          |WHERE "solution_id" = ?
          |ORDER BY "custom_group", "group_order"
          |""".stripMargin
      )

    val selectUngroupedManufacturingRecipes: Query[SolutionId, Countable[Double, RecipeId]] =
      Query(
        // language=SQL
        """SELECT
          |    "recipe_id"
          |  , "amount"
          |FROM "solution_manufacturing_recipes"
          |WHERE "solution_id" = ?
          |ORDER BY "id"
          |""".stripMargin
      )

    val updateGroupCount: Update[( Int, SolutionId )] =
      Update(
        // language=SQL
        """UPDATE "plan_solutions"
          |SET
          |    "custom_groups" = ?
          |WHERE "id" = ?
          |""".stripMargin
      )

    def selectGroupOrder(
        planId: PlanId,
        groupIndex: Int,
        rows: NonEmptyVector[Int]
    ): Query0[( SolutionId, RecipeId, Option[Int], Boolean )] =
      // language=SQL
      sql"""SELECT r."solution_id", r."recipe_id", r."group_order", r."section_before"
           |FROM       "solution_manufacturing_recipes" r
           |INNER JOIN "plan_solutions"                 s ON s."id" = r."solution_id"
           |WHERE s."plan_id" = $planId
           |  AND r."custom_group" = $groupIndex
           |  AND ${Fragments.in( fr"""r."group_order"""", rows )}
           |ORDER BY r."group_order"
           |""".stripMargin //
        .query

    def updateGroupOrder( groupNumber: Int ): Update[( SolutionId, RecipeId, Int, Boolean )] =
      Update[( Int, Boolean, SolutionId, RecipeId, Int )](
        // language=SQL
        """UPDATE "solution_manufacturing_recipes"
          |SET
          |  "group_order" = ?
          |, "section_before" = ?
          |WHERE "solution_id" = ?
          |  AND "recipe_id" = ?
          |  AND "custom_group" = ? 
          |""".stripMargin
      ).contramap { case ( solId, recId, ord, sec ) => ( ord, sec, solId, recId, groupNumber ) }

    def toggleSectionBefore( planId: PlanId, groupNumber: Int, groupRow: Int ): Update0 =
      // language=SQL
      sql"""UPDATE "solution_manufacturing_recipes" r
           |SET
           |  "section_before" = NOT "section_before"
           |FROM "plan_solutions" s
           |WHERE s."id" = r."solution_id"
           |  AND s."plan_id" = $planId
           |  AND r."custom_group" = $groupNumber
           |  AND r."group_order" = $groupRow
           |""".stripMargin //
        .update
  }
}
