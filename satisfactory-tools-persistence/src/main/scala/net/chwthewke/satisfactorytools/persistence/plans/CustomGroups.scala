package net.chwthewke.satisfactorytools
package persistence
package plans

import cats.syntax.apply._
import cats.syntax.functor._
import cats.syntax.functorFilter._
import doobie._
import doobie.implicits._

import data.ClassName
import data.Countable
import protocol.PlanId
import protocol.SolutionHeader
import protocol.SolutionId

object CustomGroups {

  def readCustomGroupSelection( planId: PlanId ): ConnectionIO[Map[ClassName, Int]] =
    (
      readPlanCustomGroups( planId ),
      ReadModel.readRecipes
    ).mapN {
      case ( groups, recipes ) =>
        groups.assignments
          .flatMap {
            case ( recipeId, ix ) => recipes.get( recipeId ).map( _.className ).tupleRight( ix )
          }
    }

  private[plans] def readPlanCustomGroups( planId: PlanId ): ConnectionIO[CustomGroupLists] =
    Headers
      .readPlanHeader( planId )
      .subflatMap( _.solution.valueAndCount )
      .semiflatMap { case ( solutionId, groupCount ) => readCustomGroups( solutionId, groupCount ) }
      .getOrElse( CustomGroupLists.empty( SolutionHeader.DefaultCustomGroups ) )

  def writeCustomGroupOrder( planId: PlanId, group: Int, groupRow: Int ): ConnectionIO[Unit] =
    statements.updateGroupOrder( planId, group, groupRow ).run.void

  def readCustomGroups( solutionId: SolutionId, groupCount: Int ): ConnectionIO[CustomGroupLists] =
    statements.selectRecipeGroups
      .toQuery0( solutionId )
      .stream
      .mapFilter { case ( recipe, groupOpt ) => groupOpt.tupleRight( recipe ) }
      .groupAdjacentBy( _._1 )
      .compile
      .fold( Vector.fill( groupCount )( Vector.empty[RecipeId] ) ) {
        case ( acc, ( groupIx, chunk ) ) => acc.updated( groupIx - 1, chunk.map( _._2 ).toVector )
      }
      .map( CustomGroupLists( _ ) )

  def updateCustomGroupSelection( planId: PlanId, groups: Map[ClassName, Int] ): ConnectionIO[Unit] =
    Headers
      .readPlanHeader( planId )
      .subflatMap( _.solution.valueAndCount )
      .semiflatMap {
        case ( solutionId, groupCount ) =>
          for {
            recipeIds <- ReadModel.readRecipeIds
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

    val selectRecipeGroups: Query[SolutionId, ( RecipeId, Option[Int] )] =
      Query(
        // language=SQL
        """SELECT
          |    "recipe_id"
          |  , "custom_group"
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

    def updateGroupOrder( planId: PlanId, groupIndex: Int, groupRow: Int ): Update0 =
      // language=SQL
      sql"""UPDATE "solution_manufacturing_recipes" r
           |SET
           |  "group_order" = ${2 * groupRow + 1} - r."group_order"
           |FROM "plan_solutions" s
           |WHERE s."id"           = r."solution_id"
           |  AND s."plan_id"      = $planId
           |  AND r."custom_group" = $groupIndex
           |  AND "group_order"    IN ( $groupRow, ${groupRow + 1} )
           |""".stripMargin //
      .update
  }
}
