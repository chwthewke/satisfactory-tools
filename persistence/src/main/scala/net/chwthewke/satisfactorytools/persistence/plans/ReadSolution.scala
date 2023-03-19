package net.chwthewke.satisfactorytools
package persistence
package plans

import cats.Foldable
import cats.Order.catsKernelOrderingForOrder
import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.functorFilter._
import cats.syntax.semigroup._
import cats.syntax.traverse._
import doobie._
import doobie.implicits._

import data.Countable
import data.Item
import model.Bill
import model.GroupAssignments
import model.Machine
import model.Recipe
import prod.ClockedRecipe
import prod.Factory
import protocol.CustomGroupResult
import protocol.ItemIO
import protocol.ItemSrcDest
import protocol.OutputTab
import protocol.PlanId
import protocol.SolutionId

object ReadSolution {

  def readPlanResult[D](
      planId: PlanId,
      solutionId: SolutionId,
      outputTab: OutputTab.Aux[D]
  ): ConnectionIO[D] =
    outputTab match {
      case OutputTab.CustomGroup( ix ) => getGroupResult( planId, solutionId, ix )
      case OutputTab.Steps             => getSolution( planId, solutionId )
      case OutputTab.Items             => getItems( planId, solutionId )
      case OutputTab.Machines          => getMachines( planId, solutionId )
      case OutputTab.Inputs            => getRawInputs( planId, solutionId )
    }

  private def getGroupResult( planId: PlanId, solutionId: SolutionId, group: Int ): ConnectionIO[CustomGroupResult] = {
    ( ReadSolverInputs.getBill( planId ), getSolution( planId, solutionId ) ).mapN {
      case ( bill, ( factory, groups ) ) =>
        val groupRecipes: Vector[Countable[Double, Recipe]] =
          factory.manufacturing.filter( r => groups.get( r.item.className ).contains( group ) )
        val external = groupRecipes
          .foldMap( _.flatTraverse( _.itemsPerMinute ) )
          .gather

        val ( input, output ) = external.partition( _.amount < 0 )

        def groupFactory: Factory =
          Factory(
            Vector.empty,
            groupRecipes,
            Countable( input, -1d ).flatSequence,
            output
          )

        def adaptItemSrcDest( otherGroup: Int => ItemSrcDest.IntraGroup )(
            srcDest: ItemSrcDest
        ): ItemSrcDest.IntraGroup =
          srcDest match {
            case step @ ItemSrcDest.Step( _, stepGroup ) =>
              if (stepGroup == group) step else otherGroup( stepGroup )

            case other: ItemSrcDest.IntraGroup => other
          }

        def inGroup( srcDest: ItemSrcDest ): Boolean =
          srcDest match {
            case ItemSrcDest.Step( _, stepGroupOpt ) => stepGroupOpt == group
            case _                                   => false
          }

        def itemIO: Map[Item, ItemIO[ItemSrcDest.IntraGroup]] =
          extractItemIO( bill, factory, groups )
            .flatMap {
              case ( item, itemIO ) =>
                Option.when(
                  (itemIO.sources ++ itemIO.destinations).exists( sd => inGroup( sd.item ) )
                ) {
                  val groupSources: Vector[Countable[Double, ItemSrcDest.IntraGroup]] =
                    itemIO.sources
                      .map( _.map( adaptItemSrcDest( ItemSrcDest.FromGroup ) ) )
                      .gather
                  val groupDestinations: Vector[Countable[Double, ItemSrcDest.IntraGroup]] =
                    itemIO.destinations
                      .map( _.map( adaptItemSrcDest( ItemSrcDest.ToGroup ) ) )
                      .gather

                  ( item, ItemIO( groupSources, groupDestinations ) )
                }
            }

        def machines: Vector[Countable[Int, Machine]] = extractMachines( groupFactory )

        CustomGroupResult(
          group,
          groupFactory,
          itemIO,
          machines
        )

    }
  }

  private def getSolution( planId: PlanId, solutionId: SolutionId ): ConnectionIO[( Factory, GroupAssignments )] =
    (
      readModelIds( planId, ReadModel.readItems ),
      readModelIds( planId, ReadModel.readRecipes ),
      statements.selectExtractionRecipes.toQuery0( solutionId ).to[Vector],
      statements.selectManufacturingRecipes.toQuery0( solutionId ).to[Vector],
      statements.selectExtraInputs.toQuery0( solutionId ).to[Vector],
      statements.selectExtraOutputs.toQuery0( solutionId ).to[Vector]
    ).mapN {
      case ( itemsById, recipesById, extraction, manufacturing, inputs, outputs ) =>
        val extractionRecipes: Vector[ClockedRecipe] =
          extraction.mapFilter {
            case ( r, s ) =>
              r.traverse( recipesById.get ).map( ClockedRecipe.overclocked( _, s ) )
          }

        val manufacturingRecipesWithGroups: Vector[( Countable[Double, Recipe], Option[Int] )] =
          manufacturing.mapFilter {
            case ( r, g ) =>
              r.traverse( recipesById.get ).tupleRight( g )
          }

        val groups: GroupAssignments = GroupAssignments(
          manufacturingRecipesWithGroups.flatMap { case ( r, g ) => g.tupleLeft( r.item.className ) }.toMap
        )

        val extraInputs: Vector[Countable[Double, Item]] =
          inputs.mapFilter( _.traverse( itemsById.get ) )

        val extraOutputs: Vector[Countable[Double, Item]] =
          outputs.mapFilter( _.traverse( itemsById.get ) )

        (
          Factory(
            extractionRecipes,
            manufacturingRecipesWithGroups.map( _._1 ),
            extraInputs,
            extraOutputs
          ),
          groups
        )
    }

  // TODO see if these can be optimized with purpose-specific SQL
  //   but it's not exactly highest priority
  private def getItems( planId: PlanId, solutionId: SolutionId ): ConnectionIO[Map[Item, ItemIO[ItemSrcDest.Global]]] =
    (
      ReadSolverInputs.getBill( planId ),
      getSolution( planId, solutionId )
    ).mapN {
      case ( bill, ( factory, groups ) ) =>
        extractItemIO( bill, factory, groups )
    }

  private def extractItemIO(
      bill: Bill,
      factory: Factory,
      groups: GroupAssignments
  ): Map[Item, ItemIO[ItemSrcDest.Global]] =
    factory.manufacturingRecipes.foldMap( manufacturingItemInOut( groups, _ ) ) |+|
      factory.extraction.foldMap( extractionItemInOut ) |+|
      itemOut( ItemSrcDest.Requested, bill.items ) |+|
      itemIn( ItemSrcDest.Input, factory.extraInputs ) |+|
      itemOut( ItemSrcDest.Byproduct, factory.extraOutputs )

  private def manufacturingItemInOut(
      groups: GroupAssignments,
      recipeBlock: ClockedRecipe
  ): Map[Item, ItemIO[ItemSrcDest.Global]] = {
    val key: ItemSrcDest.Global =
      ItemSrcDest.Step( recipeBlock.recipe.item, groups.getOrElse( recipeBlock.recipe.item.className, 0 ) )

    itemOut( key, recipeBlock.ingredientsPerMinute ) |+|
      itemIn( key, recipeBlock.productsPerMinute )
  }

  private def extractionItemInOut(
      recipeBlock: ClockedRecipe
  ): Map[Item, ItemIO[ItemSrcDest.Global]] = {
    val key: ItemSrcDest.Global =
      ItemSrcDest.Extract( recipeBlock.recipe.item )

    itemIn( key, recipeBlock.productsPerMinute )
  }

  private def itemIn[F[_]: Foldable, A <: ItemSrcDest](
      key: A,
      items: F[Countable[Double, Item]]
  ): Map[Item, ItemIO[A]] =
    items.foldMap( c => Map( c.item -> ItemIO.in( key, c.amount ) ) )

  private def itemOut[F[_]: Foldable, A <: ItemSrcDest](
      key: A,
      items: F[Countable[Double, Item]]
  ): Map[Item, ItemIO[A]] =
    items.foldMap( c => Map( c.item -> ItemIO.out( key, c.amount ) ) )

  private def getMachines( planId: PlanId, solutionId: SolutionId ): ConnectionIO[Vector[Countable[Int, Machine]]] =
    getSolution( planId, solutionId )
      .map( _._1 )
      .map( extractMachines )

  private def extractMachines( factory: Factory ): Vector[Countable[Int, Machine]] =
    factory.allRecipes
      .map( r => r.recipe.as( r.machine ) )
      .gather
      .sortBy( m => ( m.item.machineType, m.item.powerConsumption ) )

  private def getRawInputs( planId: PlanId, solutionId: SolutionId ): ConnectionIO[Vector[Countable[Double, Item]]] =
    getSolution( planId, solutionId )
      .map( _._1 )
      .map( extractRawInputs )

  private def extractRawInputs( factory: Factory ): Vector[Countable[Double, Item]] =
    factory.extraction.map( _.productsPerMinute.head ).gather

  private[plans] object statements {
    val selectManufacturingRecipes: Query[SolutionId, ( Countable[Double, RecipeId], Option[Int] )] =
      Query(
        // language=SQL
        """SELECT
          |    "recipe_id"
          |  , "amount"
          |  , "custom_group"
          |FROM "solution_manufacturing_recipes"
          |WHERE "solution_id" = ?
          |ORDER BY "custom_group", "group_order", "id"
          |""".stripMargin
      )

    val selectExtractionRecipes: Query[SolutionId, ( Countable[Int, RecipeId], Double )] =
      Query(
        // language=SQL
        """SELECT
          |    "recipe_id"
          |  , "amount"
          |  , "clock_speed"
          |FROM "solution_extraction_recipes"
          |WHERE "solution_id" = ?
          |ORDER BY "id"
          |""".stripMargin
      )

    val selectExtraInputs: Query[SolutionId, Countable[Double, ItemId]] =
      Query(
        // language=SQL
        """SELECT
          |    "item_id"
          |  , "amount"
          |FROM "solution_extra_inputs"
          |WHERE "solution_id" = ?
          |""".stripMargin
      )

    val selectExtraOutputs: Query[SolutionId, Countable[Double, ItemId]] =
      Query(
        // language=SQL
        """SELECT
          |    "item_id"
          |  , "amount"
          |FROM "solution_extra_outputs"
          |WHERE "solution_id" = ?
          |""".stripMargin
      )

  }

}
