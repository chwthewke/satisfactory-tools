package net.chwthewke.satisfactorytools

import alleycats.std.iterable._
import atto.Atto._
import atto._
import cats.Eval
import cats.Monoid
import cats.Show
import cats.data.NonEmptyList
import cats.data.NonEmptyVector
import cats.data.State
import cats.data.StateT
import cats.data.ValidatedNel
import cats.effect.Blocker
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.implicits._
import com.flowtick.graphs.Graph
import com.flowtick.graphs.Node
import com.flowtick.graphs.algorithm._
import io.circe.Decoder
import mouse.any._
import mouse.option._
import pureconfig.ConfigSource
import pureconfig.module.catseffect.syntax._

import data.GameData
import data.Loader
import data.MapConfig
import data.ProductionConfig
import model._
import prod._

object Explore extends IOApp {

  override def run( args: List[String] ): IO[ExitCode] =
    Blocker[IO]
      .use( blocker => ( loadData[GameData]( blocker ), loadMapConfig( blocker ), loadConfig( blocker ) ).tupled )
//
//      .map( _._1 )
//      .map( showItems )
//      .map( showFuelValues )
//      .map( splitItemClassNames )
//      .map( showExtractors )
//      .map( showManufacturers )
//      .map( showRecipeIngredientsAndProducts )
//
      .map( dmc => ( dmc._1, dmc._2 ) )
//      .map( (showSortedRecipeNodes _).tupled )
//      .map( showModelWith( _, showRecipesWithFluidAmounts ) )
      .map { case ( d, m ) => showModelWith( d, m, _.show ) }
//      .map( showItemSortedByDepths )
//
//      .map( (showExtractedResources _).tupled )
//      .map( showExtractedItems )
//      .map( showSelfExtractionRecipes )
//      .map( (showRecipeMatrix _).tupled )
      .flatMap( m => IO( println( m ) ) )
      .as( ExitCode.Success )

  def loadData[A: Decoder: Monoid]( blocker: Blocker ): IO[A] =
    Loader.io.loadResource[A]( blocker )

  def loadConfig( blocker: Blocker ): IO[ProductionConfig] =
    ConfigSource.default.loadF[IO, ProductionConfig]( blocker )

  def loadMapConfig( blocker: Blocker ): IO[MapConfig] =
    ConfigSource.resources( "map.conf" ).loadF[IO, MapConfig]( blocker )

  def filterRecipes( model: GameData ): Vector[Recipe[List[ClassName], ClassName]] =
    model.recipes.mapFilter(
      recipe =>
        if (recipe.producedIn.exists( model.manufacturers.keySet ) && !recipe.displayName.toLowerCase
              .startsWith( "alternate" ))
          Some( recipe )
        else if (recipe.producedIn.contains( Extractor.converterClass ))
          Some( recipe.copy( ingredients = Nil ) )
        else
          None
    )

  def showManufacturers( model: GameData ): String =
    model.manufacturers.values
      .map {
        case Manufacturer( className, displayName, powerConsumption ) =>
          show"""$displayName # $className
                |Power Consumption: $powerConsumption MW
                |""".stripMargin
      }
      .intercalate( "\n" )

  def showExtractors( model: GameData ): String =
    model.extractors.values
      .map( _._2 )
      .map {
        case Extractor(
            className,
            displayName,
            allowedResourceForms,
            allowedResources,
            powerConsumption,
            cycleTime,
            itemsPerCycle
            ) =>
          show"""$displayName  # $className
                |Allowed forms: ${allowedResourceForms.map( _.show ).intercalate( ", " )}
                |Allowed resources: ${allowedResources.cata( rs => rs.map( _.show ).intercalate( ", " ), "any" )}
                |Power: ${f"$powerConsumption%.0f"} MW
                |$itemsPerCycle items / $cycleTime
                |""".stripMargin
      }
      .intercalate( "\n" )

  def showExtractedResources( data: GameData, mapConfig: MapConfig, config: ProductionConfig ): String =
    showModelWith(
      data,
      mapConfig,
      model =>
        Calculator.computeFactory( model, config, Options.default, RecipeMatrix ).map { factory =>
          factory.blocks
            .collect {
              case FactoryBlock( Countable( recipe, amount ), _ ) if recipe.isExtraction =>
                val product = recipe.productsPerMinute.head
                ( product.item.displayName, product.amount * amount )
            }
            .sortBy { case ( p, x ) => ( -x, p ) }
            .map { case ( p, x ) => f"${p.padTo( 24, ' ' )} $x%.3f" }
            .intercalate( "\n" )
        }
    )

  def showExtractedItems( data: GameData, mapConfig: MapConfig ): String =
    showModelWith(
      data,
      mapConfig,
      model =>
        "Extracted items:\n" +
          model.extractedItems.map( _.show ).intercalate( "\n" )
    )

  def showSelfExtractionRecipes( data: GameData ): String =
    "Self-extraction recipes:\n\n" +
      data.recipes.filter( data.isSelfExtraction ).map( _.show ).intercalate( "\n\n" )

  def splitItemClassNames( model: GameData ): String = {
    val parseClassName: Parser[String] = {

      string( "Desc_" ) ~> satisfy( _ != '_' ).many1.map( _.mkString_( "" ) ) <~ string( "_C" )
    }

    val ( invalid, parsed ) = model.items.keys.toVector.partitionEither(
      cn => parseClassName.parseOnly( cn.name ).either.leftMap( _ => cn.name )
    )

    show"""${parsed.size} items parsed OK
          |
          |${invalid.size} failures
          |
          |${invalid.mkString( "\n" )}
          |""".stripMargin

  }

  def showFuelValues( model: GameData ): String =
    model.items.values
      .filter( _.fuelValue > 0d )
      .map( it => show"${it.displayName} ${it.fuelValue} MJ" )
      .intercalate( "\n" )

  def showResourceRecipes( model: GameData ): String =
    model.recipes
      .filter(
        _.product.toList.flatMap( p => model.items.get( p.item ) ).forall( _.itemType == ItemType.Resource )
      )
      .map( _.show )
      .intercalate( "\n" )

  def showRecipesWithFluidAmounts( model: Model ): String =
    (model.manufacturingRecipes ++ model.extractionRecipes.map( _._2 )).foldMap { r =>
      val fluidLines =
        r.product
          .tupleRight( ">" )
          .concat( r.ingredients.tupleRight( "<" ) )
          .filter( _._1.item.form |> Set( Form.Gas, Form.Liquid ) )
          .map { case ( Countable( item, amount ), pfx ) => f"$pfx $amount%-6.1f ${item.displayName}" }

      NonEmptyList.fromList( fluidLines ).map( lns => show"""${r.displayName}
                                                            |  ${lns.intercalate( "\n  " )}
                                                            |
                                                            |""".stripMargin )
    }.orEmpty

  def allRecipes( model: Model ): Vector[Recipe[Machine, Item]] =
    model.extractionRecipes.map( _._2 ) ++ model.manufacturingRecipes

  def makeGraph( proto: GameData, mapConfig: MapConfig ): ValidatedNel[String, Graph[Unit, RecipeGraph.N]] =
    proto.toModel( mapConfig ).map( m => RecipeGraph.of( allRecipes( m ) ).graph )

  def showModelWith[A: Show]( data: GameData, mapConfig: MapConfig, f: Model => A ): String =
    data
      .toModel( mapConfig )
      .fold(
        errs => ("Errors transforming graph:" :: errs).intercalate( "\n  " ),
        f andThen (_.show)
      )

  def showGraphWith[A: Show]( proto: GameData, mapConfig: MapConfig, f: Graph[Unit, RecipeGraph.N] => A ): String =
    makeGraph( proto, mapConfig ).fold(
      errs => ("Errors transforming graph:" :: errs).intercalate( "\n  " ),
      graph => f( graph ).show
    )

  def showGraph( proto: GameData, mapConfig: MapConfig ): String =
    showGraphWith( proto, mapConfig, graph => showNodes( graph, graph.nodes ) )

  def showSortedGraph( proto: GameData, mapConfig: MapConfig ): String =
    showGraphWith( proto, mapConfig, graph => showNodes( graph, new TopologicalSort( graph ).sort ) )

  def showSortedRecipeNodes( proto: GameData, mapConfig: MapConfig ): String = {
    def cn( className: ClassName ): String = ("\"" + className.show + "\"").padTo( 64, ' ' )
    showGraphWith(
      proto,
      mapConfig,
      graph =>
        graph.topologicalSort
          .map( _.value )
          .collect { case RecipeGraph.RecipeNode( recipe ) => recipe }
          .map( r => show"${cn( r.className )} # ${r.displayName}" )
          .intercalate( "\n" )
    )
  }

  def showRecipeIngredientsAndProducts( proto: GameData ): String = {

    show"""Recipes:
          |${proto.recipes.map( _.show ).intercalate( "\n" )}
          |""".stripMargin

  }

  def showNodes( graph: Graph[Unit, RecipeGraph.N], nodes: Iterable[Node[RecipeGraph.N]] ): String =
    nodes
      .map(
        n => show"${n.value} -> ${graph.successors( n.id ).to( Iterable ).map( _.value.show ).intercalate( ", " )}"
      )
      .intercalate( "\n" )

  def showItems( proto: GameData, mapConfig: MapConfig ): String = {
    def cn( className: ClassName ): String = ("\"" + className.show + "\":").padTo( 43, ' ' ) + "0.0                  "
    proto
      .toModel( mapConfig )
      .fold(
        errs => ("Errors transforming graph:" :: errs).intercalate( "\n  " ),
        m =>
          allRecipes( m )
            .flatMap( _.product.toList.toVector )
            .map( _.item )
            .distinct
            .map( item => show"${cn( item.className )} # ${item.displayName}" )
            .sorted
            .intercalate( "\n" )
      )
  }

  def report( recipeMatrix: RecipeMatrix ): String = {
    def okOr[A]( err: Either[String, A] ): String = err.fold( identity[String], _ => "OK" )

    show"""Unproduceable items: ${okOr( recipeMatrix.unreachableItems )}
          |
          |Indeterminate matrix: ${okOr( recipeMatrix.invertible )}
          |""".stripMargin
  }

  def showRecipeMatrix( data: GameData, mapConfig: MapConfig, config: ProductionConfig ): String =
    showModelWith(
      data,
      mapConfig,
      model => {
        val recipeMatrix = MkRecipeMatrix( config, model )
        show"""$recipeMatrix
              |
              |${report( recipeMatrix )}
              |""".stripMargin
      }
    )

  def showItemSortedByDepths( data: GameData, mapConfig: MapConfig ): String =
    showModelWith(
      data,
      mapConfig,
      model => {
        val recipeGraph = RecipeGraph.of( allRecipes( model ) )
        val depths      = computeDepths( recipeGraph )
        val sortedNodes = recipeGraph.graph.nodes.toVector.sortBy( n => depths.getOrElse( n.value, Int.MaxValue ) )
        sortedNodes
          .map( _.value )
          .collect {
            case n @ RecipeGraph.ItemNode( item ) =>
              show"${item.displayName.padTo( 36, ' ' )} ${depths( n )}"
          }
          .intercalate( "\n" )
      }
    )

  def index( model: GameData, recipes: Vector[Recipe[ClassName, ClassName]] ): Vector[ClassName] =
    recipes.flatMap( r => r.ingredients ++ r.product.toList ).map( _.item ).filter( model.items.keySet )

  def computeDepths( recipeGraph: RecipeGraph ): Map[RecipeGraph.N, Int] = {
    import recipeGraph.graph

    import prod.RecipeGraph.ItemNode
    import prod.RecipeGraph.N
    import prod.RecipeGraph.RecipeNode

    implicit def show[A: Show]: Show[Node[A]] = Show.show( node => node.value.show )

    val depths: Map[N, Int] = {
      def seenAnd( seen: Set[RecipeNode], x: N ): Set[RecipeNode] =
        x match {
          case ItemNode( _ )       => seen
          case r @ RecipeNode( _ ) => seen + r
        }

      def wasSeen( seen: Set[RecipeNode] ): N => Boolean = {
        case ItemNode( _ )       => false
        case r @ RecipeNode( _ ) => seen( r )
      }

      def succMerge( x: N ): ( Int, Int ) => Int =
        x match {
          case ItemNode( _ )   => _ min _
          case RecipeNode( _ ) => _ max _
        }

      def zero( x: N ): Int =
        x match {
          case ItemNode( _ )   => 1
          case RecipeNode( _ ) => 0
        }

      def computeFromSuccessors(
          seen: Set[RecipeNode],
          succs: NonEmptyVector[Node[N]],
          op: ( Int, Int ) => Int
      ): State[Map[N, Int], Int] =
        succs
          .traverse(
            x =>
              StateT.liftF[Eval, Map[N, Int], Unit]( Eval.always( println( show"$x REC" ) ) ) *> compute(
                seenAnd( seen, x.value ),
                x
              )
          )
          .map( _.reduceLeft( op ) + 1 )

      def compute( seen: Set[RecipeNode], node: Node[N] ): State[Map[N, Int], Int] =
        StateT.liftF[Eval, Map[N, Int], Unit]( Eval.always( println( show"$node ASK" ) ) ) *>
          State
            .get[Map[N, Int]]
            .flatMap(
              m =>
                m.get( node.value )
                  .map(
                    x => StateT.liftF[Eval, Map[N, Int], Int]( Eval.always( println( show"$node MEMO $x" ) ).as( x ) )
                  )
                  .orElse(
                    NonEmptyVector
                      .fromVector( graph.successors( node.id ).filterNot( n => wasSeen( seen )( n.value ) ).toVector )
                      .map( computeFromSuccessors( seen, _, succMerge( node.value ) ) )
                  )
                  .getOrElse( State.pure[Map[N, Int], Int]( zero( node.value ) ) )
                  .flatTap(
                    d =>
                      StateT.liftF[Eval, Map[N, Int], Unit]( Eval.always( println( show"$node PUT $d" ) ) ) *>
                        State.modify( _ + (node.value -> d) )
                  )
            )

      graph.nodes.traverse_( x => compute( seenAnd( Set.empty, x.value ), x ) ).runS( Map.empty ).value
    }

    depths
  }

}
