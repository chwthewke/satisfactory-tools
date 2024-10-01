package net.chwthewke.satisfactorytools
package persistence
package plans

import cats.Foldable
import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.functorFilter._
import cats.syntax.semigroup._
import cats.syntax.traverse._
import doobie._
import doobie.implicits._
import scala.annotation.nowarn

import data.ClassName
import data.Countable
import data.Item
import model.Bill
import model.GroupAssignment
import model.GroupAssignments
import model.Machine
import model.Recipe
import prod.ClockedRecipe
import prod.Factory
import prod.tree.FactoryTree
import protocol.CustomGroupResult
import protocol.ItemIO
import protocol.ItemSrcDest
import protocol.OutputTab
import protocol.PlanId
import protocol.SolutionId

object ReadSolution {

  @nowarn( "msg=unreachable code" )
  def readPlanResult[D](
      planId: PlanId,
      solutionId: SolutionId,
      groupCount: Int,
      outputTab: OutputTab.Aux[D]
  ): ConnectionIO[D] =
    outputTab match {
      case OutputTab.CustomGroup( ix, _ ) => getGroupResult( planId, solutionId, groupCount, ix )
      case OutputTab.Steps( _ )           => getSolution( planId, solutionId, groupCount )
      case OutputTab.GroupIO              => getGroupIO( planId, solutionId, groupCount )
      case OutputTab.Items                => getItems( planId, solutionId, groupCount )
      case OutputTab.Machines             => getMachines( planId, solutionId, groupCount )
      case OutputTab.Inputs               => getRawInputs( planId, solutionId, groupCount )
      case OutputTab.Tree                 => getPlanTree( planId, solutionId, groupCount )
    }

  private def getPlanTree( planId: PlanId, solutionId: SolutionId, groupCount: Int ): ConnectionIO[FactoryTree] =
    (
      getSolution( planId, solutionId, groupCount ),
      PlanTrees.readPlanTreeCommands( planId )
    ).mapN { case ( ( factory, _ ), commands ) => FactoryTree( factory.allRecipes ).runAll( commands ) }

  private def getGroupResult(
      planId: PlanId,
      solutionId: SolutionId,
      groupCount: Int,
      groupNumber: Int
  ): ConnectionIO[CustomGroupResult] = {
    ( ReadSolverInputs.getBill( planId ), getSolution( planId, solutionId, groupCount ) ).mapN {
      case ( bill, ( factory, groups ) ) =>
        val group: GroupAssignment[ClassName] = groups.at( groupNumber ).getOrElse( GroupAssignment.empty )

        val groupRecipes: Vector[Countable[Double, Recipe]] =
          factory.manufacturing.filter( r => group.contains( r.item.className ) )
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
              if (stepGroup == groupNumber) step else otherGroup( stepGroup )

            case other: ItemSrcDest.IntraGroup => other
          }

        def inGroup( srcDest: ItemSrcDest ): Boolean =
          srcDest match {
            case ItemSrcDest.Step( _, stepGroupOpt ) => stepGroupOpt == groupNumber
            case _                                   => false
          }

        def itemIO: Map[Item, ItemIO[ItemSrcDest.IntraGroup]] =
          extractItemIO( bill, factory, groups )
            .flatMap {
              case ( item, itemIO ) =>
                Option.when(
                  ( itemIO.sources ++ itemIO.destinations ).exists( sd => inGroup( sd.item ) )
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
          groupNumber,
          groupFactory,
          group,
          itemIO,
          machines
        )

    }
  }

  private def getGroupIO(
      planId: PlanId,
      solutionId: SolutionId,
      groupCount: Int
  ): ConnectionIO[Map[Item, ItemIO[ItemSrcDest.InterGroup]]] =
    ( ReadSolverInputs.getBill( planId ), getSolution( planId, solutionId, groupCount ) ).mapN {
      case ( bill, ( factory, groups ) ) =>
        def adaptItemSrcDest( otherGroup: Int => ItemSrcDest.InterGroup )(
            srcDest: ItemSrcDest.Global
        ): ItemSrcDest.InterGroup =
          srcDest match {
            case ItemSrcDest.Step( _, group )  => otherGroup( group )
            case ItemSrcDest.Extract( _ )      => ItemSrcDest.Input
            case other: ItemSrcDest.InterGroup => other
          }

        extractItemIO( bill, factory, groups )
          .map {
            case ( item, itemIO ) =>
              val sources: Vector[Countable[Double, ItemSrcDest.InterGroup]] =
                itemIO.sources.map( _.map( adaptItemSrcDest( ItemSrcDest.FromGroup ) ) ).gather
              val destinations: Vector[Countable[Double, ItemSrcDest.InterGroup]] =
                itemIO.destinations.map( _.map( adaptItemSrcDest( ItemSrcDest.ToGroup ) ) ).gather

              val intraGroupCancellation: Map[Int, Double] =
                ( 0 +: groups.groupsByItem.values.toVector ).distinct
                  .mapFilter( n =>
                    (
                      sources.collectFirst { case Countable( ItemSrcDest.FromGroup( `n` ), amount ) => amount },
                      destinations.collectFirst { case Countable( ItemSrcDest.ToGroup( `n` ), amount ) => amount }
                    ).mapN( math.min ).tupleLeft( n )
                  )
                  .toMap

              def extractGroup: ItemSrcDest => Option[Int] = {
                case ItemSrcDest.FromGroup( n ) => Some( n )
                case ItemSrcDest.ToGroup( n )   => Some( n )
                case _                          => None
              }

              def removeCancellations(
                  srcDests: Vector[Countable[Double, ItemSrcDest.InterGroup]]
              ): Vector[Countable[Double, ItemSrcDest.InterGroup]] =
                srcDests.map( srcDest =>
                  extractGroup( srcDest.item )
                    .flatMap( intraGroupCancellation.get )
                    .foldLeft( srcDest )( ( c, d ) => c.mapAmount( _ - d ) )
                )

              ( item, ItemIO( removeCancellations( sources ), removeCancellations( destinations ) ) )
          }
    }

  private def getSolution(
      planId: PlanId,
      solutionId: SolutionId,
      groupCount: Int
  ): ConnectionIO[( Factory, GroupAssignments[ClassName] )] =
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

        val manufacturingRecipesWithGroups: Vector[( Countable[Double, Recipe], ( Option[Int], Boolean ) )] =
          manufacturing.mapFilter {
            case ( r, g, b ) =>
              r.traverse( recipesById.get ).tupleRight( ( g, b ) )
          }

        val groups: GroupAssignments[ClassName] =
          manufacturingRecipesWithGroups.foldLeft( GroupAssignments.emptyGroups[ClassName]( groupCount ) ) {
            case ( acc, ( r, ( g, b ) ) ) => g.fold( acc )( n => acc.append( n, r.item.className, b ) )
          }

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
  private def getItems(
      planId: PlanId,
      solutionId: SolutionId,
      groupCount: Int
  ): ConnectionIO[Map[Item, ItemIO[ItemSrcDest.Global]]] =
    (
      ReadSolverInputs.getBill( planId ),
      getSolution( planId, solutionId, groupCount )
    ).mapN {
      case ( bill, ( factory, groups ) ) =>
        extractItemIO( bill, factory, groups )
    }

  private def extractItemIO(
      bill: Bill,
      factory: Factory,
      groups: GroupAssignments[ClassName]
  ): Map[Item, ItemIO[ItemSrcDest.Global]] =
    factory.manufacturingRecipes.foldMap( manufacturingItemInOut( groups, _ ) ) |+|
      factory.extraction.foldMap( extractionItemInOut ) |+|
      itemOut( ItemSrcDest.Requested, bill.items ) |+|
      itemIn( ItemSrcDest.Input, factory.extraInputs ) |+|
      itemOut( ItemSrcDest.Byproduct, factory.extraOutputs )

  private def manufacturingItemInOut(
      groups: GroupAssignments[ClassName],
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

  private def getMachines(
      planId: PlanId,
      solutionId: SolutionId,
      groupCount: Int
  ): ConnectionIO[Vector[Countable[Int, Machine]]] =
    getSolution( planId, solutionId, groupCount )
      .map( _._1 )
      .map( extractMachines )

  private def extractMachines( factory: Factory ): Vector[Countable[Int, Machine]] =
    factory.allRecipes
      .map( r => Countable( r.machine, r.machineCount ) )
      .gather
      .sortBy( m => ( m.item.machineType, m.item.powerConsumption ) )

  private def getRawInputs(
      planId: PlanId,
      solutionId: SolutionId,
      groupCount: Int
  ): ConnectionIO[Vector[Countable[Double, Item]]] =
    getSolution( planId, solutionId, groupCount )
      .map( _._1 )
      .map( extractRawInputs )

  private def extractRawInputs( factory: Factory ): Vector[Countable[Double, Item]] =
    factory.extraction.map( _.productsPerMinute.head ).gather

  private[plans] object statements {
    val selectManufacturingRecipes: Query[SolutionId, ( Countable[Double, RecipeId], Option[Int], Boolean )] =
      Query(
        // language=SQL
        """SELECT
          |    "recipe_id"
          |  , "amount"
          |  , "custom_group"
          |  , "section_before"
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
