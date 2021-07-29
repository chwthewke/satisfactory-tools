package net.chwthewke.satisfactorytools
package persistence

import alleycats.std.iterable._
import cats.Foldable
import cats.Order.catsKernelOrderingForOrder
import cats.data.OptionT
import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.functorFilter._
import cats.syntax.semigroup._
import cats.syntax.traverse._
import doobie._
import doobie.implicits._
import doobie.postgres.implicits._
import java.time.Instant

import api.PlannerApi
import data.ClassName
import data.Countable
import data.Item
import model.Bill
import model.ExtractorType
import model.Machine
import model.Options
import model.Recipe
import model.RecipeList
import model.ResourceDistrib
import model.ResourceOptions
import model.ResourcePurity
import model.ResourceWeights
import prod.Calculator
import prod.ClockedRecipe
import prod.ConstraintSolver
import prod.Factory
import prod.SolverInputs
import protocol.CustomGroupResult
import protocol.InputTab
import protocol.ItemIO
import protocol.ItemSrcDest
import protocol.OutputTab
import protocol.PlanHeader
import protocol.PlanId
import protocol.PlanName
import protocol.SolutionHeader
import protocol.SolutionId
import protocol.UserId

/*
 *  TODO Models are immutable (per-version),
 *   so it would really be more efficient to cache the Model instances somehow...
 *   maybe even cache them in the web app?
 */
// TODO also OMG such a large file
object Plans extends PlannerApi[ConnectionIO] {

  private val Tolerance: Double        = 1e-4
  private val DefaultCustomGroups: Int = 4
  private val MaxCustomGroups: Int     = 16

  override def newPlan(
      userId: UserId,
      options: Options,
      resourceOptions: ResourceOptions
  ): ConnectionIO[PlanId] =
    for {
      planId  <- statements.insertUnnamedPlan.withUniqueGeneratedKeys[PlanId]( "id" )( userId )
      itemIds <- ReadModel.readItemIds
      _       <- updateOptions( planId, Options.default )
      _       <- updateResourceOptions( planId, itemIds, resourceOptions )
    } yield planId

  override def addCustomGroup( planId: PlanId ): ConnectionIO[Boolean] =
    getPlanHeader( planId )
      .subflatMap( extractSolutionIdWithCount )
      .filter( _._1 < MaxCustomGroups )
      .semiflatMap {
        case ( groupCount, solutionId ) =>
          statements.updateGroupCount.run( ( groupCount + 1, solutionId ) )
      }
      .fold( false )( _ > 0 )

  override def removeCustomGroup( planId: PlanId ): ConnectionIO[Boolean] =
    getPlanHeader( planId )
      .subflatMap( extractSolutionIdWithCountAndLast )
      .filter { case ( groupCount, lastGroup, _ ) => groupCount > lastGroup }
      .semiflatMap {
        case ( groupCount, _, solutionId ) =>
          statements.updateGroupCount.run( ( groupCount - 1, solutionId ) )
      }
      .fold( false )( _ > 0 )

  override def setBill( planId: PlanId, bill: Bill ): ConnectionIO[Unit] =
    statements.deleteBill.run( planId ) *>
      insertBill( planId, bill )

  override def setRecipeList( planId: PlanId, recipeList: RecipeList ): ConnectionIO[Unit] =
    statements.deleteRecipeList.run( planId ) *>
      insertRecipeList( planId, recipeList )

  override def setOptions( planId: PlanId, options: Options ): ConnectionIO[Unit] =
    updateOptions( planId, options )

  override def setResourceOptions( planId: PlanId, resourceOptions: ResourceOptions ): ConnectionIO[Unit] =
    ReadModel.readItemIds
      .flatMap( updateResourceOptions( planId, _, resourceOptions ) )

  override def setCustomGroupSelection( planId: PlanId, groups: Map[ClassName, Int] ): ConnectionIO[Unit] =
    getPlanHeader( planId )
      .subflatMap( extractSolutionIdWithCount )
      .semiflatMap {
        case ( groupCount, solutionId ) =>
          for {
            recipeIds <- ReadModel.readRecipeIds
            groupSelection = groups.flatMap { case ( cn, ix ) => recipeIds.get( cn ).tupleRight( ix ) }
            rows <- groupManufacturingRecipes( solutionId, groupCount, groupSelection )
            _    <- statements.insertSolutionManufacturingRecipe.updateMany( rows )
          } yield ()
      }
      .value
      .void

  override def getPlanHeader( planId: PlanId ): OptionT[ConnectionIO, PlanHeader] =
    OptionT( statements.selectPlanHeader.toQuery0( planId ).option )

  override def getPlanQuery( planId: PlanId, inputTab: InputTab ): ConnectionIO[inputTab.Data] =
    getPlanQueryAux( planId, inputTab )

  override def getPlanResult(
      planId: PlanId,
      solutionId: SolutionId,
      outputTab: OutputTab
  ): ConnectionIO[outputTab.Data] =
    getPlanResultAux( planId, solutionId, outputTab )

  override def getCustomGroupSelection( planId: PlanId ): ConnectionIO[Map[ClassName, Int]] =
    (
      getPlanCustomGroups( planId ),
      ReadModel.readRecipes
    ).mapN {
      case ( ( _, recipeIdGroups ), recipes ) =>
        recipeIdGroups.toVector.mapFilter {
          case ( recipeId, ix ) => recipes.get( recipeId ).map( _.className ).tupleRight( ix )
        }.toMap
    }

  override def computePlan( planId: PlanId ): ConnectionIO[Unit] =
    for {
      model        <- ReadModel.readModel( ModelVersion ) // TODO actually uses only extraction recipes
      solverInputs <- getSolverInputs( planId )
      _            <- writeSolution( planId, Calculator.computeFactory( model, solverInputs, ConstraintSolver ) )
    } yield ()

  private def insertBill( planId: PlanId, bill: Bill ): ConnectionIO[Unit] =
    ReadModel.readItemIds.flatMap { itemIds =>
      val rows = bill.items
        .mapFilter( _.traverse( it => itemIds.get( it.className ) ) )
        .tupleLeft( planId )

      statements.insertBill.updateMany( rows )
    }.void

  private def insertRecipeList( planId: PlanId, recipeList: RecipeList ): ConnectionIO[Unit] =
    ReadModel.readRecipeIds.flatMap { recipeIds =>
      val rows: Vector[( PlanId, RecipeId )] =
        recipeList.recipes
          .mapFilter( re => recipeIds.get( re.className ) )
          .tupleLeft( planId )

      statements.insertRecipeList.updateMany( rows )
    }.void

  private def updateOptions( planId: PlanId, options: Options ): ConnectionIO[Unit] =
    statements.upsertOptions.run( ( planId, options ) ).void

  private def updateResourceOptions(
      planId: PlanId,
      itemIds: Map[ClassName, ItemId],
      resourceOptions: ResourceOptions
  ): ConnectionIO[Unit] =
    updateResourceWeights( planId, itemIds, resourceOptions.resourceWeights ) *>
      updateResourceDistribution( planId, itemIds, resourceOptions.resourceNodes )

  private def updateResourceWeights(
      planId: PlanId,
      itemIds: Map[ClassName, ItemId],
      resourceWeights: ResourceWeights
  ): ConnectionIO[Unit] =
    statements.deleteResourceWeights.run( planId ) *>
      statements.insertResourceWeights
        .updateMany(
          resourceWeights.weights.toVector
            .mapFilter {
              case ( item, amount ) =>
                itemIds.get( item.className ).map( ( planId, _, amount ) )
            }
        )
        .void

  private def updateResourceDistribution(
      planId: PlanId,
      itemIds: Map[ClassName, ItemId],
      resourceDistribOptions: Map[ExtractorType, Map[Item, ResourceDistrib]]
  ): ConnectionIO[Unit] = {
    val rows: Iterable[( PlanId, ExtractorType, ItemId, ResourcePurity, Int )] = for {
      ( extractor, dists ) <- resourceDistribOptions
      ( item, dist )       <- dists
      ( purity, amount ) <- Vector(
                             ( ResourcePurity.Impure, dist.impureNodes ),
                             ( ResourcePurity.Normal, dist.normalNodes ),
                             ( ResourcePurity.Pure, dist.pureNodes )
                           )
      itemId <- itemIds.get( item.className )
    } yield ( planId, extractor, itemId, purity, amount )

    statements.upsertResourceDistribution.updateMany( rows ).void
  }

  private def getPlanQueryAux[D]( planId: PlanId, tab: InputTab.Aux[D] ): ConnectionIO[D] =
    tab match {
      case InputTab.BillTab            => getBill( planId )
      case InputTab.RecipeListTab      => getRecipeList( planId )
      case InputTab.OptionsTab         => getOptions( planId )
      case InputTab.ResourceOptionsTab => getResourceOptions( planId )
    }

  private def getBill( planId: PlanId ): ConnectionIO[Bill] =
    ( ReadModel.readItems, statements.selectBillItems.toQuery0( planId ).to[Vector] )
      .mapN( ( items, billRows ) => Bill( billRows.mapFilter( _.traverse( items.get ) ) ) )

  private def getRecipeList( planId: PlanId ): ConnectionIO[RecipeList] =
    ( ReadModel.readRecipes, statements.selectRecipeListItems.toQuery0( planId ).to[Vector] )
      .mapN( ( recipes, recipeList ) => RecipeList( recipeList.mapFilter( recipes.get ) ) )

  private def getOptions( planId: PlanId ): ConnectionIO[Options] =
    statements.selectOptions.toQuery0( planId ).unique

  private def translateMapKeys[I, K, A]( iMap: Map[I, A], kMap: Map[I, K] ): Map[K, A] =
    for {
      ( i, a ) <- iMap
      k        <- kMap.get( i )
    } yield ( k, a )

  private def getResourceOptions( planId: PlanId ): ConnectionIO[ResourceOptions] =
    (
      ReadModel.readItems,
      statements.selectResourceNodes.toQuery0( planId ).stream.compile.foldMonoid,
      statements.selectResourceWeights.toQuery0( planId ).stream.compile.foldMonoid
    ).mapN(
      ( itemsById, nodes, weights ) =>
        ResourceOptions(
          nodes.map { case ( ex, dists ) => ( ex, translateMapKeys( dists, itemsById ) ) },
          ResourceWeights( translateMapKeys( weights, itemsById ) )
        )
    )

  private def getPlanResultAux[D](
      planId: PlanId,
      solutionId: SolutionId,
      outputTab: OutputTab.Aux[D]
  ): ConnectionIO[D] =
    outputTab match {
      case OutputTab.CustomGroup( ix ) => getGroupResult( solutionId, ix )
      case OutputTab.Plan              => getSolution( solutionId )
      case OutputTab.Items             => getItems( planId, solutionId )
      case OutputTab.Machines          => getMachines( solutionId )
      case OutputTab.Inputs            => getRawInputs( solutionId )
    }

  private def getGroupResult( solutionId: SolutionId, group: Int ): ConnectionIO[CustomGroupResult] =
    ( ReadModel.readRecipes, statements.selectGroupManufacturingRecipes.toQuery0( ( solutionId, group ) ).to[Vector] )
      .mapN {
        case ( recipesById, groupRecipes ) =>
          val recipes: Vector[Countable[Double, Recipe]] =
            groupRecipes.mapFilter( _.traverse( recipesById.get ) )

          val external = recipes
            .foldMap( _.flatTraverse( _.itemsPerMinute ) )
            .gather
            .filter( _.amount.abs > Tolerance )

          val ( input, output ) = external.partition( _.amount < 0 )

          val factory = Factory(
            Vector.empty,
            recipes,
            Countable( input, -1d ).flatSequence,
            output
          )

          CustomGroupResult(
            factory,
            extractItemIO( Bill.empty, factory, ItemSrcDest.Output ),
            extractMachines( factory )
          )

      }

  private def getSolution( solutionId: SolutionId ): ConnectionIO[( Factory, Map[ClassName, Int] )] =
    (
      ReadModel.readItems,
      ReadModel.readRecipes,
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

        val groups: Map[ClassName, Int] =
          manufacturingRecipesWithGroups.flatMap { case ( r, g ) => g.tupleLeft( r.item.className ) }.toMap

        val extraInputs: Vector[Countable[Double, Item]] =
          inputs.mapFilter( _.traverse( itemsById.get ) )

        val extraOutputs =
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
  private def getItems( planId: PlanId, solutionId: SolutionId ): ConnectionIO[Map[Item, ItemIO]] =
    (
      getBill( planId ),
      getSolution( solutionId )
        .map( _._1 )
    ).mapN( extractItemIO( _, _, ItemSrcDest.Byproduct ) )

  private type ItemIOBySrcDest =
    ( Map[ItemSrcDest, Double], Map[ItemSrcDest, Double] )

  private def extractItemIO( bill: Bill, factory: Factory, extraOutput: ItemSrcDest ): Map[Item, ItemIO] =
    (factory.allRecipes.foldMap( itemInOut ) |+|
      itemOut( ItemSrcDest.Requested, bill.items ) |+|
      itemIn( ItemSrcDest.Input, factory.extraInputs ) |+|
      itemOut( extraOutput, factory.extraOutputs )).map {
      case ( item, ( ins, outs ) ) =>
        item ->
          ItemIO(
            ins.map { case ( isd, am )  => Countable( isd, am ) }.toVector,
            outs.map { case ( isd, am ) => Countable( isd, am ) }.toVector
          )
    }

  private def itemInOut( recipeBlock: ClockedRecipe ): Map[Item, ItemIOBySrcDest] = {
    val key = ItemSrcDest.Recipe( recipeBlock.recipe.item.displayName )

    itemIn( key, recipeBlock.ingredientsPerMinute ) |+|
      itemOut( key, recipeBlock.productsPerMinute )
  }

  private def itemIn[F[_]: Foldable](
      key: ItemSrcDest,
      items: F[Countable[Double, Item]]
  ): Map[Item, ItemIOBySrcDest] =
    items.foldMap {
      case Countable( item, amount ) =>
        Map[Item, ItemIOBySrcDest](
          (
            item,
            ( Map( key -> amount ), Map.empty )
          )
        )
    }

  private def itemOut[F[_]: Foldable](
      key: ItemSrcDest,
      items: F[Countable[Double, Item]]
  ): Map[Item, ItemIOBySrcDest] =
    items.foldMap {
      case Countable( item, amount ) =>
        Map[Item, ItemIOBySrcDest](
          (
            item,
            ( Map.empty, Map( key -> amount ) )
          )
        )
    }

  private def getMachines( solutionId: SolutionId ): ConnectionIO[Vector[Countable[Int, Machine]]] =
    getSolution( solutionId )
      .map( _._1 )
      .map( extractMachines )

  private def extractMachines( factory: Factory ): Vector[Countable[Int, Machine]] =
    factory.allRecipes
      .map( r => r.recipe.as( r.machine ) )
      .gather
      .sortBy( m => ( m.item.machineType, m.item.powerConsumption ) )

  private def getRawInputs( solutionId: SolutionId ): ConnectionIO[Vector[Countable[Double, Item]]] =
    getSolution( solutionId )
      .map( _._1 )
      .map( extractRawInputs )

  private def extractRawInputs( factory: Factory ): Vector[Countable[Double, Item]] =
    factory.extraction.map( _.productsPerMinute.head ).gather

  private def getSolverInputs( planId: PlanId ): ConnectionIO[SolverInputs] =
    ( getBill( planId ), getRecipeList( planId ), getOptions( planId ), getResourceOptions( planId ) )
      .mapN( SolverInputs( _, _, _, _ ) )

  private[persistence] def writeSolution( planId: PlanId, solution: Either[String, Factory] ): ConnectionIO[Unit] =
    for {
      solutionHeader                         <- getPlanHeader( planId ).subflatMap( extractSolutionIdWithCount ).value
      ( customGroupCount, groupsByRecipeId ) <- getCustomGroups( solutionHeader )
      _                                      <- statements.deleteSolution.run( planId )
      _ <- solution.fold(
            err => statements.insertSolutionError.run( ( planId, err ) ),
            factory => writeSolution( planId, factory, customGroupCount, groupsByRecipeId )
          )
    } yield ()

  private def extractSolutionIdWithCount( planHeader: PlanHeader ): Option[( Int, SolutionId )] =
    planHeader.solution match {
      case SolutionHeader.NotComputed                           => None
      case SolutionHeader.PlanError( _ )                        => None
      case SolutionHeader.Computed( solutionId, groupCount, _ ) => Some( ( groupCount, solutionId ) )
    }

  private def extractSolutionIdWithCountAndLast( planHeader: PlanHeader ): Option[( Int, Int, SolutionId )] =
    planHeader.solution match {
      case SolutionHeader.NotComputed                                   => None
      case SolutionHeader.PlanError( _ )                                => None
      case SolutionHeader.Computed( solutionId, groupCount, lastGroup ) => Some( ( groupCount, lastGroup, solutionId ) )
    }

  private def getCustomGroups(
      solutionHeader: Option[( Int, SolutionId )]
  ): ConnectionIO[( Int, Map[RecipeId, Int] )] =
    solutionHeader
      .foldMapM {
        case ( groupCount, solutionId ) =>
          statements.selectManufacturingRecipes
            .toQuery0( solutionId )
            .stream
            .map { case ( c, g ) => g.tupleLeft( c.item ) }
            .unNone
            .compile
            .to( Map )
            .tupleLeft( groupCount )
      }

  private def writeSolution(
      planId: PlanId,
      factory: Factory,
      customGroupCount: Int,
      groupsByRecipeId: Map[RecipeId, Int]
  ): ConnectionIO[Unit] =
    for {
      solutionId <- writeSolutionHeader( planId, customGroupCount )
      itemIds    <- ReadModel.readItemIds
      recipeIds  <- ReadModel.readRecipeIds
      _          <- writeExtractionRecipes( solutionId, recipeIds, factory.extraction )
      _          <- writeManufacturingRecipes( solutionId, recipeIds, groupsByRecipeId, factory.manufacturing )
      _          <- writeExtraInputs( solutionId, itemIds, factory.extraInputs )
      _          <- writeExtraOutputs( solutionId, itemIds, factory.extraOutputs )
    } yield ()

  private def writeSolutionHeader( planId: PlanId, customGroupCount: Int ): ConnectionIO[SolutionId] =
    statements.insertSolution
      .withUniqueGeneratedKeys[SolutionId]( "id" )(
        ( planId, if (customGroupCount == 0) DefaultCustomGroups else customGroupCount )
      )

  private def writeExtractionRecipes(
      solutionId: SolutionId,
      recipeIds: Map[ClassName, RecipeId],
      recipes: Vector[ClockedRecipe]
  ): ConnectionIO[Int] = {

    val rows: Vector[( SolutionId, RecipeId, Int, Double )] = recipes.flatMap {
      case ClockedRecipe( recipe, clockSpeed, _ ) =>
        recipeIds.get( recipe.item.className ).map( ( solutionId, _, recipe.amount, clockSpeed ) )
    }

    statements.insertSolutionExtractionRecipe.updateMany( rows )
  }

  private def writeManufacturingRecipes(
      solutionId: SolutionId,
      recipeIds: Map[ClassName, RecipeId],
      groupsByRecipeId: Map[RecipeId, Int],
      recipes: Vector[Countable[Double, Recipe]]
  ): ConnectionIO[Int] = {

    val rows: Vector[( SolutionId, RecipeId, Double, Option[Int] )] = recipes.mapFilter {
      case Countable( recipe, amount ) =>
        recipeIds
          .get( recipe.className )
          .map( recipeId => ( solutionId, recipeId, amount, groupsByRecipeId.get( recipeId ) ) )
    }

    statements.insertSolutionManufacturingRecipe.updateMany( rows )
  }

  private def writeExtraInputs(
      solutionId: SolutionId,
      itemIds: Map[ClassName, ItemId],
      extraInputs: Vector[Countable[Double, Item]]
  ): ConnectionIO[Int] =
    writeExtraInputOutputs( statements.insertSolutionExtraInput )( solutionId, itemIds, extraInputs )

  private def writeExtraOutputs(
      solutionId: SolutionId,
      itemIds: Map[ClassName, ItemId],
      extraOutputs: Vector[Countable[Double, Item]]
  ): ConnectionIO[Int] =
    writeExtraInputOutputs( statements.insertSolutionExtraOutput )( solutionId, itemIds, extraOutputs )

  private def writeExtraInputOutputs( stmt: Update[( SolutionId, ItemId, Double )] )(
      solutionId: SolutionId,
      itemIds: Map[ClassName, ItemId],
      items: Vector[Countable[Double, Item]]
  ): ConnectionIO[Int] = {
    val rows: Vector[( SolutionId, ItemId, Double )] =
      items.mapFilter {
        case Countable( item, amount ) =>
          itemIds.get( item.className ).map( ( solutionId, _, amount ) )
      }

    stmt.updateMany( rows )
  }

  private def groupManufacturingRecipes(
      solutionId: SolutionId,
      groupCount: Int,
      groupSelection: Map[RecipeId, Int]
  ): ConnectionIO[Vector[( SolutionId, RecipeId, Double, Option[Int] )]] =
    statements.selectUngroupedManufacturingRecipes
      .toQuery0( solutionId )
      .map {
        case Countable( recipe, amount ) =>
          ( solutionId, recipe, amount, groupSelection.get( recipe ).filter( _ <= groupCount ) )
      }
      .to[Vector]

  object statements {
    val insertUnnamedPlan: Update[UserId] =
      Update(
        // language=SQL
        """INSERT INTO "plans"
          |  ( "user_id" )
          |VALUES
          |  ( ? )
          |""".stripMargin
      )

    val upsertOptions: Update[( PlanId, Options )] =
      Update(
        // language=SQL
        """INSERT INTO "plan_options"
          |  ( "plan_id"
          |  , "belt_option" 
          |  , "pipe_option"
          |  , "miner_option"
          |  , "clock_speed_option"
          |  , "extractors_option"
          |  , "fracking_option"
          |  )
          |VALUES
          |  ( ?, ?, ?, ?, ?, ?, ? )
          |ON CONFLICT ON CONSTRAINT "options_plan_fkey"
          |DO UPDATE SET
          |    "belt_option"        = "excluded"."belt_option"
          |  , "pipe_option"        = "excluded"."pipe_option"
          |  , "miner_option"       = "excluded"."miner_option"
          |  , "clock_speed_option" = "excluded"."clock_speed_option"
          |  , "extractors_option"  = "excluded"."extractors_option"
          |  , "fracking_option"    = "excluded"."fracking_option"
          |""".stripMargin
      )

    val upsertResourceDistribution: Update[( PlanId, ExtractorType, ItemId, ResourcePurity, Int )] =
      Update(
        // language=SQL
        """INSERT INTO "resource_distribution_options"
          |  ( "plan_id"
          |  , "extractor_type"
          |  , "item_id"
          |  , "purity"
          |  , "amount"
          |  )
          |VALUES 
          |  ( ?, ?, ?, ?, ? )
          |ON CONFLICT ON CONSTRAINT "resource_distribution_unique"
          |DO UPDATE SET
          |    "amount" = "excluded"."amount"
          |""".stripMargin
      )

    val deleteResourceWeights: Update[PlanId] =
      Update(
        // language=SQL
        """DELETE FROM "resource_weights"
          |WHERE "plan_id" = ?
          |""".stripMargin
      )

    val insertResourceWeights: Update[( PlanId, ItemId, Int )] =
      Update(
        // language=SQL
        """INSERT INTO "resource_weights"
          |  ( "plan_id"
          |  , "item_id"
          |  , "weight"
          |  )
          |VALUES
          |  ( ?, ?, ? )
          |""".stripMargin
      )

    val deleteBill: Update[PlanId] =
      Update(
        // language=SQL
        """DELETE FROM "bill_items"
          |WHERE "plan_id" = ?
          |""".stripMargin
      )

    val insertBill: Update[( PlanId, Countable[Double, ItemId] )] =
      Update(
        // language=SQL
        """INSERT INTO "bill_items"
          |  ( "plan_id", "item_id", "amount" )
          |VALUES
          |  ( ?, ?, ? )
          |""".stripMargin
      )

    val deleteRecipeList: Update[PlanId] =
      Update(
        // language=SQL
        """DELETE FROM "recipe_lists"
          |WHERE "plan_id" = ?
          |""".stripMargin
      )

    val insertRecipeList: Update[( PlanId, RecipeId )] =
      Update(
        // language=SQL
        """INSERT INTO "recipe_lists"
          |  ( "plan_id", "recipe_id" )
          |VALUES
          |  ( ?, ? )
          |""".stripMargin
      )

    val deleteSolution: Update[PlanId] =
      Update(
        // language=SQL
        """DELETE FROM "plan_solutions"
          |WHERE "plan_id" = ?
          |""".stripMargin
      )

    val insertSolution: Update[( PlanId, Int )] =
      Update(
        // language=SQL
        """INSERT INTO "plan_solutions"
          |  ( "plan_id", "custom_groups" )
          |VALUES 
          |  ( ?, ? )
          |""".stripMargin
      )

    val insertSolutionError: Update[( PlanId, String )] =
      Update(
        // language=SQL
        """INSERT INTO "plan_solutions"
          |  ( "plan_id", "error_message", "custom_groups" ) 
          |VALUES
          |  ( ?, ?, 0 )
          |""".stripMargin
      )

    val insertSolutionExtractionRecipe: Update[( SolutionId, RecipeId, Int, Double )] =
      Update(
        // language=SQL
        """INSERT INTO "solution_extraction_recipes"
          |  ( "solution_id", "recipe_id", "amount", "clock_speed" ) 
          |VALUES
          |  ( ?, ?, ?, ? )
          |""".stripMargin
      )

    val insertSolutionManufacturingRecipe: Update[( SolutionId, RecipeId, Double, Option[Int] )] =
      Update(
        // language=SQL
        """INSERT INTO "solution_manufacturing_recipes"
          |  ( "solution_id", "recipe_id", "amount", "custom_group" ) 
          |VALUES
          |  ( ?, ?, ?, ? )
          |ON CONFLICT ON CONSTRAINT "solution_manufacturing_recipes_unique"
          |DO UPDATE
          |  SET
          |    "amount"       = "excluded"."amount"
          |  , "custom_group" = "excluded"."custom_group"
          |""".stripMargin
      )

    val insertSolutionExtraInput: Update[( SolutionId, ItemId, Double )] =
      Update(
        // language=SQL
        """INSERT INTO "solution_extra_inputs"
          |  ( "solution_id", "item_id", "amount" )
          |VALUES
          |  ( ?, ?, ? )
          |""".stripMargin
      )

    val insertSolutionExtraOutput: Update[( SolutionId, ItemId, Double )] =
      Update(
        // language=SQL
        """INSERT INTO "solution_extra_outputs"
          |  ( "solution_id", "item_id", "amount" )
          |VALUES
          |  ( ?, ?, ? )
          |""".stripMargin
      )

    type PlanHeaderRow =
      (
          Option[PlanName],
          Option[PlanId],
          Instant,
          Option[SolutionId],
          Option[String],
          Option[Int],
          Option[Int]
      )

    val selectPlanHeader: Query[PlanId, PlanHeader] =
      Query[PlanId, PlanHeaderRow](
        // language=SQL
        """SELECT 
          |    p."name"
          |  , p."src_id"
          |  , p."updated"
          |  , s."id"
          |  , s."error_message"
          |  , s."custom_groups"
          |  , MAX( r."custom_group" )
          |FROM        "plans"                          p
          |LEFT JOIN   "plan_solutions"                 s ON p."id" = s."plan_id"
          |LEFT JOIN   "solution_manufacturing_recipes" r ON s."id" = r."solution_id"
          |WHERE p."id" = ?
          |GROUP BY p."id", s."id"
          |""".stripMargin
      ).map {
        case ( nameOpt, srcIdOpt, updated, solutionIdOpt, solutionErrorOpt, groupCountOpt, lastGroupOpt ) =>
          PlanHeader(
            nameOpt,
            srcIdOpt,
            updated,
            solutionIdOpt,
            solutionErrorOpt,
            groupCountOpt,
            lastGroupOpt
          )
      }

    val selectBillItems: Query[PlanId, Countable[Double, ItemId]] =
      Query(
        // language=SQL
        """SELECT
          |    "item_id"
          |  , "amount"
          |FROM "bill_items"
          |WHERE "plan_id" = ?
          |""".stripMargin
      )

    val selectRecipeListItems: Query[PlanId, RecipeId] =
      Query(
        // language=SQL
        """SELECT "recipe_id"
          |FROM "recipe_lists"
          |WHERE "plan_id" = ?
          |""".stripMargin
      )

    val selectOptions: Query[PlanId, Options] =
      Query(
        // language=SQL
        """SELECT
          |    "belt_option"
          |  , "pipe_option"
          |  , "miner_option"
          |  , "clock_speed_option"
          |  , "extractors_option"
          |  , "fracking_option"
          |FROM "plan_options"
          |WHERE "plan_id" = ?
          |""".stripMargin
      )

    val selectResourceNodes: Query[PlanId, Map[ExtractorType, Map[ItemId, ResourceDistrib]]] =
      Query[PlanId, ( ExtractorType, ItemId, ResourcePurity, Int )](
        // language=SQL
        """SELECT
          |    "extractor_type"
          |  , "item_id"
          |  , "purity"
          |  , "amount"
          |FROM "resource_distribution_options"
          |WHERE "plan_id" = ?
          |""".stripMargin
      ).map {
        case ( extractorType, item, purity, amount ) =>
          Map( extractorType -> Map( item -> ResourceDistrib.of( purity, amount ) ) )
      }

    val selectResourceWeights: Query[PlanId, Map[ItemId, Int]] =
      Query[PlanId, ( ItemId, Int )](
        // language=SQL
        """SELECT
          |    "item_id"
          |  , "weight"
          |FROM "resource_weights"
          |WHERE "plan_id" = ?
          |""".stripMargin
      ).map { case ( itemId, amount ) => Map( itemId -> amount ) }

    val selectGroupManufacturingRecipes: Query[( SolutionId, Int ), Countable[Double, RecipeId]] =
      Query(
        // language=SQL
        """SELECT
          |    "recipe_id"
          |  , "amount"
          |FROM "solution_manufacturing_recipes"
          |WHERE "solution_id" = ?
          |  AND "custom_group" = ?
          |""".stripMargin
      )

    val selectManufacturingRecipes: Query[SolutionId, ( Countable[Double, RecipeId], Option[Int] )] =
      Query(
        // language=SQL
        """SELECT
          |    "recipe_id"
          |  , "amount"
          |  , "custom_group"
          |FROM "solution_manufacturing_recipes"
          |WHERE "solution_id" = ?
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

    val updateGroupCount: Update[( Int, SolutionId )] =
      Update(
        // language=SQL
        """UPDATE "plan_solutions"
          |SET
          |    "custom_groups" = ?
          |WHERE "id" = ?
          |""".stripMargin
      )

  }

}
