package net.chwthewke.satisfactorytools

import breeze.linalg.DenseVector
import breeze.optimize.linear.LinearProgram
import breeze.optimize.linear.LinearProgram.ApacheSimplexSolver
import cats.Semigroup
import cats.data.State
import cats.data.ValidatedNel
import cats.effect.IO
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.semigroup._
import cats.syntax.show._
import cats.syntax.traverse._
import mouse.option._

import data.ProductionConfig
import model.ClassName
import model.Countable
import model.Item
import model.Machine
import model.Model
import model.Options
import model.Recipe
import net.chwthewke.satisfactorytools.prod.RecipeSelection
import net.chwthewke.satisfactorytools.prod.ResourceCaps
import net.chwthewke.satisfactorytools.prod.ResourceWeights
import prod.Bill

object ExploreLinProg
    extends Program[ProductionConfig]( "test-linear-programming", "Sample linear programming solver with breeze" ) {

  val options = Options.full

  class LPS( val lp: LinearProgram ) {

    case class VarDict( vars: Map[LPVariable, lp.Variable], indices: Vector[LPVariable] ) {
      def get( lpVariable: LPVariable ): Option[lp.Variable] = vars.get( lpVariable )

      def put( key: LPVariable, variable: lp.Variable ): VarDict = {
        val newVars = vars + ( ( key, variable ) )
        val newIndices = indices.indexOf( key ) match {
          case -1 => indices :+ key
          case ix => indices.patch( ix, Vector( key ), 1 )
        }
        VarDict( newVars, newIndices )
      }
    }

    object VarDict {
      val empty: VarDict = VarDict( Map.empty, Vector.empty )
    }

    def getVars: State[VarDict, VarDict]                               = State.get
    def getIndices: State[VarDict, Vector[LPVariable]]                 = State.inspect( _.indices )
    def getVar( key: LPVariable ): State[VarDict, Option[lp.Variable]] = State.inspect( _.get( key ) )
    def getOrCreateVar( key: LPVariable ): State[VarDict, lp.Variable] =
      getVar( key ).flatMap(
        _.cata(
          State.pure[VarDict, lp.Variable]( _ ),
          State
            .pure[VarDict, lp.Variable]( lp.Real( key.toString ) )
            .flatTap( v => State.modify( _.put( key, v ) ) )
        )
      )

    def toLinearProblem( bill: Bill, resourceWeights: Map[Item, Double], model: Model ): State[VarDict, lp.Problem] = {

      def variableOf( lpVariable: LPVariable ): State[VarDict, lp.Variable] = getOrCreateVar( lpVariable )

      implicit val expressionSemigroup: Semigroup[lp.Expression] = new Semigroup[lp.Expression] {
        override def combine( x: lp.Expression, y: lp.Expression ): lp.Expression = x + y
      }

      def manufacturingExpressions: State[VarDict, Map[Item, lp.Expression]] =
        model.manufacturingRecipes.foldMapM { recipe =>
          variableOf( RecipeVariable( recipe ) ).map(
            recipeVar =>
              (recipe.ingredientsPerMinute.map( in => in.copy( amount = -in.amount ) ) ++
                recipe.productsPerMinute.toList)
                .map( ci => ( ci.item, recipeVar * ci.amount ) )
                .toMap
          )
        }

      def extractionVariables: State[VarDict, Map[Item, lp.Variable]] =
        model.extractedItems
          .traverse( item => variableOf( ItemVariable( item ) ).tupleLeft( item ) )
          .map( _.toMap )

      ( manufacturingExpressions, extractionVariables, State.get[VarDict] ).mapN { ( m, e, v ) =>
        val exprs = m |+| e

        val itemConstraints = exprs.map {
          case ( item, expr ) => ( item.displayName, expr >= bill.items.find( _.item == item ).fold( 0d )( _.amount ) )
        }

        println( itemConstraints.mkString( "Item Constraints: \n", "\n", "\n\n" ) )

        val recipeConstraints = v.vars.collect {
          case ( RecipeVariable( r ), v ) => ( r.displayName, v >= 0 )
        }
        println( recipeConstraints.mkString( "Recipe Constraints: \n", "\n", "\n\n" ) )

        val constraints = (itemConstraints.values ++ recipeConstraints.values).toVector

        val objective =
          e.map { case ( it, v ) => resourceWeights.get( it ).fold[lp.Expression]( v )( v * _ ) }.reduce( _ + _ )

        lp.minimize( objective ).subjectTo( constraints: _* )
      }

    }

    def solveLinearProblem(
        problem: lp.Problem,
        model: Model
    ): State[VarDict, Either[
      Throwable,
      ( Vector[Countable[Recipe[Machine, Item], Double]], Vector[Countable[Item, Double]], Double )
    ]] = {

      val rawE = Either.catchNonFatal( problem.solve( ApacheSimplexSolver ) )

      rawE.traverse( presentRawSolution( model, _ ) )
    }

    def presentRawSolution(
        model: Model,
        raw: lp.Result
    ): State[VarDict, ( Vector[Countable[Recipe[Machine, Item], Double]], Vector[Countable[Item, Double]], Double )] =
      ( model.manufacturingRecipes.foldMapM { r =>
        getVar( RecipeVariable( r ) )
          .map { v =>
            v.map( raw.valueOf )
              .filter( x => x.abs > 1e-12 )
              .map( x => Countable( r, x ) )
              .toVector
          }
      }, model.extractedItems.foldMapM { it =>
        getVar( ItemVariable( it ) )
          .map { v =>
            v.map( raw.valueOf )
              .filter( x => x.abs > 1e-12 )
              .map( x => Countable( it, x ) )
              .toVector
          }
      } ).mapN { ( m, e ) =>
        ( m, e, raw.value )
      }

    def checkResult( result: lp.Result ): ValidatedNel[String, Double] = {
      def evalConstraint( c: lp.Constraint ): ValidatedNel[String, Unit] = {
        val l = result.valueOf( c.lhs )
        val r = result.valueOf( c.rhs )

        val met = c.relation match {
          case lp.LTE => l - r < 1e-12
          case lp.EQ  => (l - r).abs < 1e-12
          case lp.GTE => r - l < 1e-12
        }

        Option.when( met )( () ).toValidNel( s"Unmet constraint: $c" )
      }

      result.problem.constraints.toVector
        .traverse_( evalConstraint )
        .as( result.value )

    }

    def makeResult(
        model: Model,
        problem: lp.Problem,
        recipes: Vector[Countable[ClassName, Double]],
        extracted: Vector[Countable[ClassName, Double]]
    ): State[VarDict, lp.Result] = {
      val actualRecipes = recipes.foldMap(
        r => model.manufacturingRecipes.find( _.className == r.item ).map( Countable( _, r.amount ) ).toVector
      )
      val actualExtracted = extracted.foldMap(
        it => model.extractedItems.find( _.className == it.item ).map( Countable( _, it.amount ) ).toVector
      )

      getIndices.map { indices =>
        val result: DenseVector[Double] = DenseVector.zeros( indices.size )

        val withRecipes = actualRecipes.foldLeft( result ) { ( ixs, r ) =>
          ixs.update( indices.indexOf( RecipeVariable( r.item ) ), r.amount )
          ixs
        }

        val withItems = actualExtracted.foldLeft( withRecipes ) { ( ixs, it ) =>
          ixs.update( indices.indexOf( ItemVariable( it.item ) ), it.amount )
          ixs
        }

        lp.Result( withItems, problem )
      }
    }
  }

  sealed trait LPVariable
  final case class RecipeVariable( recipe: Recipe[Machine, Item] ) extends LPVariable {
    override def toString: String = s"Recipe_${recipe.className.name}"
  }
  final case class ItemVariable( item: Item ) extends LPVariable {
    override def toString: String = s"Item_${item.className.name}"
  }

  def showResult(
      blocks: Vector[Countable[Recipe[Machine, Item], Double]],
      extr: Vector[Countable[Item, Double]],
      cost: Double
  ): String =
    show"""Blocks:
          |${blocks
            .map( b => f"${b.amount}%4.4f ${b.item.displayName}" )
            .intercalate( "\n" )}
          |
          |Extracted:
          |${extr.map( e => f"${e.amount}%4.4f ${e.item.displayName}" ).intercalate( "\n" )}
          |
          |Cost: ${f"$cost%.4f"}
          |""".stripMargin

  override def runProgram( model: Model, config: ProductionConfig ): IO[Unit] = {
    val configWithTestBill = config.copy( items = Vector( Countable( ClassName( "Desc_IronPlate_C" ), 60.0 ) ) )

    val bill = Bill
      .init( model, configWithTestBill )
      .leftMap( e => new IllegalArgumentException( e ) )
      .liftTo[IO]

    val recipeSelection = RecipeSelection.init( model, config, options )
    val resourceWeights =
      recipeSelection.map( sel => ResourceWeights.init( ResourceCaps.init( model, options, sel.extractionRecipes ) ) )

    val lps = new LPS( new LinearProgram )

    def mkSol( model: Model, problem: lps.lp.Problem ) =
      lps.makeResult(
        model,
        problem,
        Vector(
          Countable( ClassName( "Recipe_IronPlate_C" ), 3.25d ),
          Countable( ClassName( "Recipe_Alternate_PureIronIngot_C" ), 1.5d )
        ),
        Vector(
          Countable( ClassName( "Desc_OreIron_C" ), 52.5d ),
          Countable( ClassName( "Desc_Water_C" ), 30000d )
        )
      )

    for {
      b                      <- bill
      w                      <- resourceWeights.leftMap( new IllegalArgumentException( _ ) ).liftTo[IO]
      ( v, p )               <- IO.delay( lps.toLinearProblem( b, w.weights, model ).run( lps.VarDict.empty ).value )
      _                      <- IO.delay( println( p ) )
      ( blocks, extr, cost ) <- lps.solveLinearProblem( p, model ).runA( v ).value.liftTo[IO]
      _                      <- IO( println( showResult( blocks, extr, cost ) ) )
      r1 = mkSol( model, p ).runA( v ).value
      _ <- IO( println( s"My results constraints ${lps.checkResult( r1 )}" ) )
      ( b1, e1, c1 ) = lps.presentRawSolution( model, r1 ).runA( v ).value
      _ <- IO( println( showResult( b1, e1, c1 ) ) )
    } yield ()

  }
}
