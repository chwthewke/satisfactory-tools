package net.chwthewke.satisfactorytools

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all._

import data.ClassName
import data.GameData
import data.Schematic
import loader.Loader
import loader.ModelInit
import loader.RecipeClassifier
import loader.RecipeClassifier.AllOf
import loader.RecipeClassifier.AnalysisItem

object ExploreGameData extends IOApp {
  override def run( args: List[String] ): IO[ExitCode] =
    Loader.io
      .loadGameData( DataVersionStorage.Release1_0 )
      .flatTap( printPowerRecipes )
//      .flatTap( gd =>
//        printConverterRecipes( gd ) *>
//          IO.println( "" ) *>
//          printConstructorRecipes( gd )
//      )
//      .flatTap( data => IO.println( data ) )
//      .flatTap( data => printExtractors( data ) *> printManufacturers( data ) )
      .as( ExitCode.Success )

  def printBeltsAndPipes( data: GameData ): IO[Unit] =
    IO.println(
      data.conveyorBelts.mkString_( "\n" ) +
        "\n\n" +
        data.pipelines.mkString_( "\n" )
    )

  def printBuildingDescriptors( data: GameData ): IO[Unit] =
    IO.println( data.buildingDescriptors.values.toVector.mkString_( "\n" ) )

  def printPowerRecipes( data: GameData ): IO[Unit] =
    IO.println( ModelInit.powerRecipes( data ).mkString_( "\n\n" ) )

  def printExtractors( data: GameData ): IO[Unit] =
    IO.println( data.extractors.map { case ( _, x ) => x.show }.mkString( "EXTRACTORS\n\n", "\n", "\n\n" ) )

  def printManufacturers( data: GameData ): IO[Unit] =
    IO.println( data.manufacturers.map { case ( _, m ) => m.show }.mkString( "MANUFACTURERS\n\n", "\n", "\n\n" ) )

  def printCateriumInitAnalysisItems( data: GameData ): IO[Unit] = {
    val analyzer: RecipeClassifier#MilestoneAnalyzer = RecipeClassifier( data ).MilestoneAnalyzer.init
    IO.println( analyzer.initAnalyses.toMap.get( ClassName( "Research_Caterium_1_C" ) ) match {
      case Some( Left( _ ) ) => "LEFT???"
      case None              => "NONE???"
      case Some( Right( items ) ) =>
        items.toString + "\n" +
          items.traverse( item => None ).toString
    } )
  }

  def printConstructorRecipes( data: GameData ): IO[Unit] =
    IO.println(
      data.recipes
        .filter( _.producedIn.contains( ClassName( "Build_ConstructorMk1_C" ) ) )
        .mkString_( "\n\n" )
    )

  def printConverterRecipes( data: GameData ): IO[Unit] =
    IO.println(
      data.recipes
        .filter( _.producedIn.contains( ClassName( "Build_Converter_C" ) ) )
        .mkString_( "\n\n" )
    )

  def printQuantumEncoderRecipes( data: GameData ): IO[Unit] =
    IO.println(
      data.recipes
        .filter( _.producedIn.contains( ClassName( "Build_QuantumEncoder_C" ) ) )
        .mkString_( "\n\n" )
    )

  def printPowerGenerators( data: GameData ): IO[Unit] =
    IO.println( data.powerGenerators.values.toVector.mkString_( "\n\n" ) )

  def printFuelValues( data: GameData ): IO[Unit] =
    IO.println(
      data.powerGenerators.values.toVector
        .foldMap( _.fuels.map( _.fuel ) )
        .distinct
        .mapFilter( cn =>
          data.items.get( cn ).map {
            case ( item, _ ) => f"${item.displayName} ${item.fuelValue}%.3f"
          }
        )
        .sorted
        .mkString( "\n" )
    )

  def printDependencies( data: GameData ): IO[Unit] = {
    val analyzer: RecipeClassifier#MilestoneAnalyzer = RecipeClassifier( data ).MilestoneAnalyzer.init

    def displaySchematic( tag: String, schematic: Schematic ): String =
      show"[$tag] ${schematic.displayName} [${schematic.techTier}] # ${schematic.className}"

    val recipesTxt = analyzer.recipeSchematics
      .flatMap {
        case ( cn, s ) =>
          analyzer.manufacturingRecipes
            .find( _.className == cn )
            .map { r =>
              val m =
                r.producedIn
                  .collectFirstSome( analyzer.manufacturerSchematics.get )
                  .map( ms => show"\n  ${displaySchematic( "M", ms )}" )
                  .orEmpty

              show"""[R] ${r.displayName} # ${r.className}
                    |  ${displaySchematic( "S", s )}$m
                    |""".stripMargin
            }
      }
      .mkString( "RECIPES\n\n", "\n", "\n" )

    val schematicsTxt = analyzer.schematicDependencies
      .flatMap {
        case ( cn, ds ) =>
          data.schematics
            .find( _.className == cn )
            .map( s =>
              ds.toList
                .map( displaySchematic( "D", _ ) )
                .mkString(
                  show"""${displaySchematic( "S", s )}\n  """,
                  "\n  ",
                  ""
                )
            )
      }
      .mkString( "SCHEMATICS\n\n", "\n", "\n" )

    IO.print( recipesTxt + "\n" + schematicsTxt )
  }

  def printBaseRecipes( data: GameData ): IO[Unit] = {
    val analyzer: RecipeClassifier#MilestoneAnalyzer = RecipeClassifier( data ).MilestoneAnalyzer.init

    IO.println( analyzer.baseRecipes.flatMap {
      case ( cn, r ) =>
        data.items
          .get( cn )
          .map( _._1 )
          .map( item => show"""${item.displayName} # $cn
                              |  ${r.displayName} # ${r.className}
                              |""".stripMargin )
    }.mkString )

  }

  def printAnalysis( data: GameData ): IO[Unit] = {
    val analyzer: RecipeClassifier#MilestoneAnalyzer = RecipeClassifier( data ).MilestoneAnalyzer.init
    val initAnalysis: Map[ClassName, Either[RecipeClassifier.Milestone, AllOf[RecipeClassifier.AnalysisItem]]] =
      analyzer.initAnalyses.toMap
    val analysis: Map[ClassName, RecipeClassifier.Milestone] =
      analyzer.analyze

    val results
        : Map[ClassName, ( RecipeClassifier.Milestone, Either[RecipeClassifier.Milestone, AllOf[AnalysisItem]] )] =
      analysis.align( initAnalysis ).mapFilter( _.onlyBoth )

    val resultsTxt = results.map {
      case ( cn, ( m, srcs ) ) =>
        val srcTiers: AllOf[String] = srcs.toOption.orEmpty
          .map { item =>
            val tier = analysis.get( item.className ).fold( "-" )( _.tier.show )
            val tag = item match {
              case AnalysisItem.OfRecipe( _ )    => "R"
              case AnalysisItem.OfSchematic( _ ) => "S"
            }
            show"[$tag] ${item.displayName} $tier # ${item.className}"
          }

        srcTiers.items
          .map { alt =>
            if (alt.items.size < 2) alt.mkString_( "\n  " )
            else alt.mkString_( "  one of:\n    ", "\n    ", "" )
          }
          .mkString( show"$cn Tier ${m.tier}\n  ", "\n  ", "" )
    }

    IO.println( resultsTxt.mkString( "\n\n" ) )
  }

  def printSchematics( data: GameData ): IO[Unit] = {
    val analyzer: RecipeClassifier#MilestoneAnalyzer = RecipeClassifier( data ).MilestoneAnalyzer.init

    val recipeSchematics =
      analyzer.recipeSchematics
        .flatMap {
          case ( c, s ) =>
            data.recipes.find( _.className == c ).map( r => show"${r.displayName}\n  ${s.displayName}" )
        }

    IO.print( recipeSchematics.mkString( "\n" ) )
  }

  def printManufacturingRecipesToClassify( data: GameData ): IO[Unit] = {
    val analyzer: RecipeClassifier#MilestoneAnalyzer = RecipeClassifier( data ).MilestoneAnalyzer.init

    IO.println(
      analyzer.manufacturingRecipes.map( _.className ).sorted.mkString_( "RECIPES TO ANALYZE\n  ", "\n  ", "" )
    ) *>
      IO.println(
        analyzer.recipeSchematics
          .map {
            case ( name, schematic ) =>
              show"$name from [${schematic.`type`}]\n${schematic.show.linesIterator.map( "  " + _ ).mkString( "\n" )}"
          }
          .toVector
          .sorted
          .mkString_( "RECIPE SCHEMATICS\n  ", "\n  ", "" )
      )
  }

  def printRecipes( data: GameData ): IO[Unit] =
    IO.println(
      show"""RECIPES
            |${data.recipes
             .map( recipe =>
               show"""${recipe.displayName} [${recipe.className}] in ${recipe.producedIn.headOption}
                     |  ${recipe.ingredients
                      .map( _.item.name )
                      .mkString( ", " )} -> ${recipe.products.map( _.item.name ).mkString_( ", " )}
                     |""".stripMargin
             )
             .mkString}
            |""".stripMargin
    )

  def printItemIcons( data: GameData ): IO[Unit] =
    IO.println(
      show"""ITEM ICONS
            |${data.items.values
             .map( _._1 )
             .toVector
             .sortBy( _.displayName )
             .map( item => show"${item.displayName} => ${item.smallIcon}" )
             .mkString( "\n" )}
            |""".stripMargin
    )

  def printSchematicTypes( data: GameData ): IO[Unit] =
    IO.println(
      show"""SCHEMATIC TYPES
            |${data.schematics.map( _.`type` ).distinct.mkString_( "\n" )}
            |""".stripMargin
    )

}
