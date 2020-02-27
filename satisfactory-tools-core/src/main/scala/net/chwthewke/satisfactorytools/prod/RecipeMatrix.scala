package net.chwthewke.satisfactorytools
package prod

import alleycats.std.iterable._
import breeze.linalg.{Vector => _, _}
import cats.data.NonEmptyVector
import cats.Show
import cats.instances.double._
import cats.instances.int._
import cats.instances.map._
import cats.instances.string._
import cats.instances.vector._
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.show._
import com.flowtick.graphs.Graph
import com.flowtick.graphs.algorithm._
import mouse.boolean._
import mouse.option._
//
import model.Countable
import model.Item
import model.Machine
import model.Model
import model.Recipe

final case class RecipeMatrix(
    rowLabels: Vector[Item],
    columnLabels: Vector[Recipe[Machine, Item]],
    matrix: DenseMatrix[Double] // TODO rational?
) {
  val unreachableItems: Either[String, Unit] =
    NonEmptyVector
      .fromVector(
        rowLabels
          .filter( wanted => columnLabels.forall( recipe => recipe.product.toList.forall( _.item != wanted ) ) )
      )
      .map( uris => show"Unproduceable items:${uris.map( _.className.show ).intercalate( "," )}" )
      .toLeft( () )

  val invertible: Either[String, Unit] =
    (matrix.rows == matrix.cols)
      .either( show"Non-square matrix (${matrix.cols} recipes, ${matrix.rows} items)", () )
      .flatMap( _ => (math.abs( det( matrix ) ) > 1e-12).either( "Non-invertible matrix", () ) )

  def report: String = {
    def okOr[A]( err: Either[String, A] ): String = err.fold( identity[String], _ => "OK" )

    show"""Unproduceable items: ${okOr( unreachableItems )}
          |
          |Indeterminate matrix: ${okOr( invertible )}
          |""".stripMargin
  }

  def computeFactory( bill: Bill ): Either[String, Factory] = {

    def mkGoal: Either[String, DenseVector[Double]] =
      bill.items
        .foldMapA {
          case Countable( item, amount ) =>
            val ix = rowLabels.indexOf( item )
            (ix != -1)
              .either( item, Map( ix -> amount ) )
              .toValidatedNel
        }
        .map( byIndex => DenseVector.tabulate( matrix.rows )( ix => byIndex.getOrElse( ix, 0d ) ) )
        .toEither
        .leftMap( items => show"Requested items cannot be produced: ${items.map( _.displayName ).intercalate( ", " )}" )

    def checkSolution( counts: Vector[Double] ): Either[String, Unit] = {
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
    }

    for {
      // TODO move health checks into construction?
      _ <- unreachableItems
      _ <- invertible
      //
      goal     <- mkGoal
      solution <- solve( goal )
      _        <- checkSolution( solution )
    } yield Factory(
      columnLabels.zip( solution ).map { case ( recipe, count ) => FactoryBlock( Countable( recipe, count ) ) }
    )
  }

  def solve( v: DenseVector[Double] ): Either[String, Vector[Double]] =
    Either
      .catchNonFatal( matrix \ v )
      .leftMap( err => s"Solving for required items failed: ${err.getMessage}" )
      .map( _.toScalaVector() )
}

object RecipeMatrix {

  private def showMatrixRow( matrix: Matrix[Double], rowIx: Int ): String =
    (0 until matrix.cols).map( colIx => f"${matrix( rowIx, colIx )}%.4f" ).to( Iterable ).intercalate( "," )

  private def showMatrixRows( matrix: Matrix[Double], rowLabels: Vector[Item] ): String =
    rowLabels
      .zip( 0 until matrix.rows )
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

  def init( productionConfig: ProductionConfig, model: Model ): RecipeMatrix = {

    val activeRecipes: Vector[Recipe[Machine, Item]] =
      productionConfig.recipes
        .flatMap( cn => model.recipes.find( _.className == cn ) )
    val wantedItems: Vector[Countable[Item, Double]] =
      productionConfig.items
        .filter( _.amount != 0d )
        .flatMap { case Countable( cn, amount ) => model.items.get( cn ).map( Countable( _, amount ) ) }

    val graph: Graph[Unit, RecipeGraph.N, Unit] = RecipeGraph.of( activeRecipes )

    val orderedNodes = graph.topologicalSort

    val ( producedItems, usedRecipes ): ( Vector[Item], Vector[Recipe[Machine, Item]] ) = {
      val reachable =
        new DepthFirstSearch[Unit, RecipeGraph.N, Unit](
          wantedItems.map( ci => RecipeGraph.ItemNode( ci.item ) ),
          graph
        ).run.toVector

      reachable.foldLeft( ( Vector.empty[Item], Vector.empty[Recipe[Machine, Item]] ) ) {
        case ( ( itAcc, recAcc ), node ) =>
          node match {
            case RecipeGraph.ItemNode( item )     => ( itAcc :+ item, recAcc )
            case RecipeGraph.RecipeNode( recipe ) => ( itAcc, recAcc :+ recipe )
          }
      }
    }

    val recipeOrder = orderedNodes.collect {
      case RecipeGraph.RecipeNode( recipe ) => recipe
    }

    val itemOrder = orderedNodes.collect {
      case RecipeGraph.ItemNode( item ) => item
    }

    val recipes = usedRecipes.sortBy( r => -recipeOrder.indexOf( r ) )
    val items   = producedItems.sortBy( i => -itemOrder.indexOf( i ) )

    RecipeMatrix(
      items,
      recipes,
      DenseMatrix.tabulate( items.size, recipes.size )(
        ( i, j ) =>
          recipes( j ).ingredientsPerMinute
            .find( _.item == items( i ) )
            .cata( -_.amount, 0d ) +
            recipes( j ).productsPerMinute
              .find( _.item == items( i ) )
              .cata( _.amount, 0d )
      )
    )
  }

}
