package net.chwthewke.satisfactorytools
package persistence

import cats.syntax.apply._
import cats.syntax.functor._
import cats.syntax.traverse._
import doobie._
import doobie.implicits._
import scala.annotation.nowarn
import scala.concurrent.duration.FiniteDuration

import data.ClassName
import data.Countable
import data.Item
import model.ExtractorType
import model.Machine
import model.Model
import model.Power
import model.Recipe
import model.ResourceDistrib
import model.ResourcePurity

@nowarn( "cat=lint-byname-implicit" )
object WriteModel {

  implicit val powerWrite: Write[Power] =
    Write[( Double, Option[Double] )].contramap {
      case Power.Fixed( value )       => ( value, None )
      case Power.Variable( min, max ) => ( min, Some( max ) )
    }

  private[persistence] def insertItem( version: Int ): Update[Item] =
    Update[( Item, Int )](
      // language=SQL
      """INSERT INTO "items"
        |( "class_name"
        |, "display_name"
        |, "form"
        |, "energy_value"
        |, "sink_points"
        |, "data_version"
        |)
        |VALUES ( ?, ?, ?, ?, ?, ? )
        |""".stripMargin
    ).contramap( ( _, version ) )

  private[persistence] def insertMachine( version: Int ): Update[Machine] =
    Update[( Machine, Int )](
      // language=SQL
      """INSERT INTO "machines"
        |( "class_name"
        |, "display_name"
        |, "machine_type"
        |, "power_consumption"
        |, "data_version"
        |)
        |VALUES ( ?, ?, ?, ?, ? )
        |""".stripMargin
    ).contramap( ( _, version ) )

  private[persistence] def insertRecipe( version: Int ): Update[( Recipe, MachineId )] =
    Update[( ClassName, String, FiniteDuration, MachineId, Power, Int )](
      // language=SQL
      """INSERT INTO "recipes"
        |( "class_name"
        |, "display_name"
        |, "duration_ms"
        |, "produced_in"
        |, "power"
        |, "power_var"
        |, "data_version"
        |)
        |VALUES ( ?, ?, ?, ?, ?, ?, ? )
        |""".stripMargin
    ).contramap {
      case ( recipe, machineId ) =>
        ( recipe.className, recipe.displayName, recipe.duration, machineId, recipe.power, version )
    }

  private[persistence] val insertRecipeIngredient: Update[( RecipeId, Countable[Double, ItemId] )] =
    Update(
      // language=SQL
      """INSERT INTO "recipe_ingredients"
        |( "recipe_id"
        |, "item_id"
        |, "amount"
        |)
        |VALUES ( ?, ?, ? )
        |""".stripMargin
    )

  private[persistence] val insertRecipeProduct: Update[( RecipeId, Countable[Double, ItemId] )] =
    Update(
      // language=SQL
      """INSERT INTO "recipe_products"
        |( "recipe_id"
        |, "item_id"
        |, "amount"
        |)
        |VALUES ( ?, ?, ? )
        |""".stripMargin
    )

  private[persistence] val insertExtractionRecipe: Update[( ItemId, ResourcePurity, RecipeId )] =
    Update(
      // language=SQL
      """INSERT INTO "extraction_recipes"
        |( "item_id"
        |, "purity"
        |, "recipe_id"
        |)
        |VALUES ( ?, ?, ? )
        |""".stripMargin
    )

  private[persistence] def insertResourceNodes( version: Int ): Update[( ExtractorType, ItemId, ResourceDistrib )] =
    Update[( ExtractorType, ItemId, ResourceDistrib, Int )](
      // language=SQL
      """INSERT INTO "resource_nodes"
        |( "extractor_type"
        |, "item_id"
        |, "impure"
        |, "normal"
        |, "pure"
        |, "data_version"
        |)
        |VALUES ( ?, ?, ?, ?, ?, ? )
        |""".stripMargin
    ).contramap {
      case ( extractorType, itemId, distrib ) => ( extractorType, itemId, distrib, version )
    }

  private def writeItems( items: Vector[Item], version: Int ): ConnectionIO[Map[ClassName, ItemId]] =
    insertItem( version )
      .updateManyWithGeneratedKeys[ItemId]( "id" )( items )
      .compile
      .toVector
      .map( ids => items.map( _.className ).zip( ids ).toMap )

  private def writeMachines( machines: Vector[Machine], version: Int ): ConnectionIO[Map[ClassName, MachineId]] =
    insertMachine( version )
      .updateManyWithGeneratedKeys[MachineId]( "id" )( machines )
      .compile
      .toVector
      .map( ids => machines.map( _.className ).zip( ids ).toMap )

  private def writeRecipes(
      recipes: Vector[Recipe],
      machines: Map[ClassName, MachineId],
      itemIds: Map[ClassName, ItemId],
      version: Int
  ): ConnectionIO[Map[ClassName, RecipeId]] = {
    val recipesWithIds = recipes.flatMap( recipe => machines.get( recipe.producedIn.className ).tupleLeft( recipe ) )

    for {
      recipeIds <- insertRecipe( version )
                    .updateManyWithGeneratedKeys[RecipeId]( "id" )( recipesWithIds )
                    .compile
                    .toVector
                    .map( recipesWithIds.map( _._1 ).zip )
      _ <- writeRecipeIngredients( recipeIds, itemIds )
      _ <- writeRecipeProducts( recipeIds, itemIds )
    } yield recipeIds.map { case ( recipe, id ) => ( recipe.className, id ) }.toMap

  }

  private def writeRecipeIngredients(
      recipes: Vector[( Recipe, RecipeId )],
      itemIds: Map[ClassName, ItemId]
  ): ConnectionIO[Unit] =
    insertRecipeIngredient
      .updateMany(
        recipes.flatMap {
          case ( recipe, id ) =>
            recipe.ingredients.flatMap( _.traverse( item => itemIds.get( item.className ) ) ).toVector.map( ( id, _ ) )
        }
      )
      .void

  private def writeRecipeProducts(
      recipes: Vector[( Recipe, RecipeId )],
      itemIds: Map[ClassName, ItemId]
  ): ConnectionIO[Unit] =
    insertRecipeProduct
      .updateMany(
        recipes.flatMap {
          case ( recipe, id ) =>
            recipe.products.toList
              .flatMap( _.traverse( item => itemIds.get( item.className ) ) )
              .toVector
              .map( ( id, _ ) )
        }
      )
      .void

  private def writeExtractionRecipes(
      extractionRecipes: Vector[( Item, ResourcePurity, Recipe )],
      recipeIds: Map[ClassName, RecipeId],
      itemIds: Map[ClassName, ItemId]
  ): ConnectionIO[Unit] =
    insertExtractionRecipe
      .updateMany(
        extractionRecipes.flatMap {
          case ( item, purity, recipe ) =>
            ( itemIds.get( item.className ), recipeIds.get( recipe.className ) ).mapN( ( _, purity, _ ) )
        }
      )
      .void

  private def writeResourceNodes(
      resourceNodes: Map[ExtractorType, Map[Item, ResourceDistrib]],
      itemIds: Map[ClassName, ItemId],
      version: Int
  ): ConnectionIO[Unit] =
    insertResourceNodes( version )
      .updateMany(
        resourceNodes.toVector.flatMap {
          case ( ex, map ) =>
            map.toVector.flatMap { case ( item, distrib ) => itemIds.get( item.className ).map( ( ex, _, distrib ) ) }
        }
      )
      .void

  def writeModel( model: Model, version: Int ): ConnectionIO[Unit] = {
    val allRecipes  = model.manufacturingRecipes ++ model.extractionRecipes.map( _._3 )
    val allMachines = allRecipes.map( _.producedIn ).distinctBy( _.className )
    for {
      itemIds    <- writeItems( model.items.values.toVector, version )
      machineIds <- writeMachines( allMachines, version )
      recipeIds  <- writeRecipes( allRecipes, machineIds, itemIds, version )
      _          <- writeExtractionRecipes( model.extractionRecipes, recipeIds, itemIds )
      _          <- writeResourceNodes( model.defaultResourceOptions.resourceNodes, itemIds, version )
    } yield ()
  }

}
