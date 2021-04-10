package net.chwthewke.satisfactorytools
package prod

import alleycats.std.iterable._
import cats.Show
import cats.data.NonEmptyVector
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.show._
import com.flowtick.graphs.algorithm._
import mouse.boolean._
import org.ojalgo.matrix.Primitive64Matrix

import model.Countable
import model.Item
import model.Machine
import model.Recipe

final case class RecipeMatrix(
    rowLabels: Vector[Item],
    columnLabels: Vector[Recipe[Machine, Item]],
    matrix: Primitive64Matrix
) {
  val unreachableItems: Either[String, Unit] =
    NonEmptyVector
      .fromVector(
        rowLabels
          .filter( wanted => columnLabels.forall( recipe => recipe.product.toList.forall( _.item != wanted ) ) )
      )
      .fproduct(
        items => columnLabels.filter( recipe => recipe.ingredients.exists( ingr => items.contains_( ingr.item ) ) )
      )
      .map {
        case ( items, recipes ) =>
          show"""Unproduceable items:
                |  ${items.map( _.displayName.show ).intercalate( "\n  " )}
                |Recipes requiring unproduceable items:
                |  ${recipes.map( _.displayName.show ).intercalate( "\n  " )}
                |""".stripMargin
      }
      .toLeft( () )

  def nonUniquePlan: String =
    show"""Too ${if (columnLabels.size > rowLabels.size) "many" else "few"} recipes (${columnLabels.size})
          |
          |${columnLabels.map( _.displayName ).intercalate( "\n" )}
          |
          |to produce ${rowLabels.size} items
          |
          |${rowLabels.map( _.displayName ).intercalate( "\n" )}
          |""".stripMargin

  val invertible: Either[String, Unit] = {
    matrix.isSquare
      .either( nonUniquePlan, () )
      .flatMap( _ => (math.abs( matrix.getDeterminant.get ) > 1e-12).either( "Non-invertible matrix", () ) )
  }

  def computeFactory( bill: Bill ): Either[String, Solution] = {

    def mkGoal: Either[String, Vector[Double]] =
      bill.items
        .foldMapA {
          case Countable( item, amount ) =>
            val ix = rowLabels.indexOf( item )
            (ix != -1)
              .either( item, Map( ix -> amount ) )
              .toValidatedNel
        }
        .map( byIndex => rowLabels.indices.map( ix => byIndex.getOrElse( ix, 0d ) ).toVector )
        .toEither
        .leftMap( items => show"Requested items cannot be produced: ${items.map( _.displayName ).intercalate( ", " )}" )

    def checkSolution( counts: Vector[Double] ): Either[String, Unit] =
      counts.zipWithIndex
        .traverse_(
          ci =>
            (ci._1 >= -1e-12)
              .either(
                columnLabels( ci._2 ).displayName,
                ()
              )
              .toValidatedNel
        )
        .leftMap( inv => s"Error: recipes had negative counts: ${inv.intercalate( ", " )}" )
        .toEither

    for {
      // TODO move health checks into construction?
      _ <- unreachableItems
      _ <- invertible
      //
      goal     <- mkGoal
      solution <- solve( goal )
      _        <- checkSolution( solution )
    } yield {

      val factoryBlocks =
        columnLabels.zip( solution ).map {
          case ( recipe, count ) => Countable( recipe, count )
        }

      Solution( factoryBlocks, Vector.empty )
    }
  }

  def solve( v: Vector[Double] ): Either[String, Vector[Double]] = {
    val rhs = Primitive64Matrix.FACTORY.columns( v.toArray )

    Either
      .catchNonFatal( matrix.solve( rhs ) )
      .leftMap( err => s"Solving for required items failed: ${err.getMessage}" )
      .map( x => rowLabels.indices.map( i => x.get( i.toLong, 0 ).toDouble ).toVector )
  }
}

object RecipeMatrix extends Solver {

  override def solve(
      bill: Bill,
      recipes: RecipeSelection,
      resourceWeights: ResourceWeights,
      resourceCaps: ResourceCaps
  ): Either[String, Solution] =
    init( bill, recipes ).computeFactory( bill )

  private def showMatrixRow( matrix: Primitive64Matrix, rowIx: Long ): String =
    (0L until matrix.countColumns)
      .map( colIx => f"${matrix.get( rowIx, colIx )}%.4f" )
      .to( Iterable )
      .intercalate( "," )

  private def showMatrixRows( matrix: Primitive64Matrix, rowLabels: Vector[Item] ): String =
    rowLabels
      .zip( 0L until matrix.countRows )
      .map {
        case ( lab, rowIx ) =>
          show"${lab.displayName}:${showMatrixRow( matrix, rowIx )}"
      }
      .intercalate( "\n" )

  implicit val recipeMatrixShow: Show[RecipeMatrix] =
    Show.show( mat => show"""Recipe Matrix: ${mat.rowLabels.size} items x ${mat.columnLabels.size} recipes
                            |Recipes: ${mat.columnLabels.map( _.displayName ).intercalate( "," )}
                            |${showMatrixRows( mat.matrix, mat.rowLabels )}
                            |""".stripMargin )

  def init( bill: Bill, recipeSelection: RecipeSelection ): RecipeMatrix =
    init(
      recipeSelection.allowedRecipes ++ recipeSelection.extractionRecipes.values.toVector,
      bill.items.map( _.item )
    )

  def init( activeRecipes: Vector[Recipe[Machine, Item]], wantedItems: Vector[Item] ): RecipeMatrix = {
    val recipeGraph: RecipeGraph = RecipeGraph.of( activeRecipes )

    val orderedNodes = recipeGraph.graph.topologicalSort

    val ( producedItems, usedRecipes ): ( Vector[Item], Vector[Recipe[Machine, Item]] ) = {
      val reachable =
        new DepthFirstSearch[Unit, RecipeGraph.N](
          wantedItems.map( item => RecipeGraph.itemNode( item ).show ),
          recipeGraph.graph
        ).run.toVector

      reachable.foldLeft( ( Vector.empty[Item], Vector.empty[Recipe[Machine, Item]] ) ) {
        case ( ( itAcc, recAcc ), node ) =>
          node.value match {
            case RecipeGraph.ItemNode( item )     => ( itAcc :+ item, recAcc )
            case RecipeGraph.RecipeNode( recipe ) => ( itAcc, recAcc :+ recipe )
          }
      }
    }

    val recipeOrder = orderedNodes.map( _.value ).collect {
      case RecipeGraph.RecipeNode( recipe ) => recipe
    }

    val itemOrder = orderedNodes.map( _.value ).collect {
      case RecipeGraph.ItemNode( item ) => item
    }

    val recipes = usedRecipes.sortBy( r => -recipeOrder.indexOf( r ) )
    val items   = producedItems.sortBy( i => -itemOrder.indexOf( i ) )

    val matrix = Primitive64Matrix.FACTORY.columns(
      recipes.map(
        recipe => items.map( item => recipe.reducedItemsPerMinute.getOrElse( item, 0d ) ).toArray
      ): _*
    )

    RecipeMatrix( items, recipes, matrix )
  }

}
