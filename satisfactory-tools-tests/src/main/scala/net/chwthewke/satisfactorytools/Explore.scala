package net.chwthewke.satisfactorytools

import alleycats.std.iterable._
import cats.Monoid
import cats.Show
import cats.data.ValidatedNel
import cats.effect.Blocker
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.implicits._
import com.flowtick.graphs.Graph
import com.flowtick.graphs.algorithm._
import io.circe.Decoder
import mouse.option._
import pureconfig.ConfigSource
import pureconfig.module.catseffect.syntax._
//
import load.Loader
import model._
import prod._

object Explore extends IOApp {

  override def run( args: List[String] ): IO[ExitCode] =
    Blocker[IO]
      .use( blocker => ( loadData[ProtoModel]( blocker ), loadConfig( blocker ) ).tupled )
//
      .map( _._1 )
//      .map( showItems )
//      .map( showSortedRecipeNodes )
      .map( showModelWith( _, _.show ) )
//      .map( _.show )
//
//      .map( (showRecipeMatrix _).tupled )
      .flatMap( m => IO( println( m ) ) )
      .as( ExitCode.Success )

  def loadData[A: Decoder: Monoid]( blocker: Blocker ): IO[A] =
    Loader.io.loadResource[A]( blocker )

  def loadConfig( blocker: Blocker ): IO[ProductionConfig] =
    ConfigSource.default.loadF[IO, ProductionConfig]( blocker )

  def filterRecipes( model: ProtoModel ): Vector[Recipe[ClassName, ClassName]] =
    model.recipes.mapFilter(
      recipe =>
        if (recipe.producers.exists( Manufacturer.builders ) && !recipe.displayName.toLowerCase
              .startsWith( "alternate" ))
          Some( recipe )
        else if (recipe.producers.contains( Extractor.extractorClass ))
          Some( recipe.copy( ingredients = Nil ) )
        else
          None
    )

  def showExtractors( model: ProtoModel ): String =
    model.extractors.values
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

  def showResourceRecipes( model: ProtoModel ): String =
    model.recipes
      .filter(
        _.product.toList.flatMap( p => model.items.get( p.item ) ).forall( _.itemType == ItemType.Resource )
      )
      .map( _.show )
      .intercalate( "\n" )

  def makeGraph( proto: ProtoModel ): ValidatedNel[String, Graph[Unit, RecipeGraph.N, Unit]] =
    proto.toModel.map( m => RecipeGraph.of( m.recipes ) )

  def showModelWith[A: Show]( data: ProtoModel, f: Model => A ): String =
    data.toModel.fold(
      errs => ("Errors transforming graph:" :: errs).intercalate( "\n  " ),
      f andThen (_.show)
    )

  def showGraphWith[A: Show]( proto: ProtoModel, f: Graph[Unit, RecipeGraph.N, Unit] => A ): String =
    makeGraph( proto ).fold(
      errs => ("Errors transforming graph:" :: errs).intercalate( "\n  " ),
      graph => f( graph ).show
    )

  def showGraph( proto: ProtoModel ): String =
    showGraphWith( proto, graph => showNodes( graph, graph.nodes ) )

  def showSortedGraph( proto: ProtoModel ): String =
    showGraphWith( proto, graph => showNodes( graph, new TopologicalSort( graph ).sort ) )

  def showSortedRecipeNodes( proto: ProtoModel ): String = {
    def cn( className: ClassName ): String = ("\"" + className.show + "\"").padTo( 64, ' ' )
    showGraphWith(
      proto,
      graph =>
        graph.topologicalSort
          .collect { case RecipeGraph.RecipeNode( recipe ) => recipe }
          .map( r => show"${cn( r.className )} # ${r.displayName}" )
          .intercalate( "\n" )
    )
  }

  def showNodes( graph: Graph[Unit, RecipeGraph.N, Unit], nodes: Iterable[RecipeGraph.N] ): String =
    nodes
      .map( n => show"$n -> ${graph.successors( n ).to( Iterable ).map( _.show ).intercalate( ", " )}" )
      .intercalate( "\n" )

  def showItems( proto: ProtoModel ): String = {
    def cn( className: ClassName ): String = ("\"" + className.show + "\":").padTo( 43, ' ' ) + "0.0                  "
    proto.toModel
      .fold(
        errs => ("Errors transforming graph:" :: errs).intercalate( "\n  " ),
        _.recipes
          .flatMap( _.product.toList.toVector )
          .map( _.item )
          .distinct
          .map( item => show"${cn( item.className )} # ${item.displayName}" )
          .sorted
          .intercalate( "\n" )
      )
  }

  def showRecipeMatrix( data: ProtoModel, config: ProductionConfig ): String =
    showModelWith(
      data,
      model => {
        val recipeMatrix = RecipeMatrix.init( config, model )
        show"""$recipeMatrix
              |
              |${recipeMatrix.report}
              |""".stripMargin
      }
    )

  def index( model: ProtoModel, recipes: Vector[Recipe[ClassName, ClassName]] ): Vector[ClassName] =
    recipes.flatMap( r => r.ingredients ++ r.product.toList ).map( _.item ).filter( model.items.keySet )

}
