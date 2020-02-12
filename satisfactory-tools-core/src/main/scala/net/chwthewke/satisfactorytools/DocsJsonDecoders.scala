package net.chwthewke.satisfactorytools

import atto.Parser
import cats.data.NonEmptyList
import cats.data.OptionT
import cats.instances.either._
import cats.syntax.applicative._
import cats.syntax.option._
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Json
import mouse.boolean._

object DocsJsonDecoders {
  val recipeClass   = "Class'/Script/FactoryGame.FGRecipe'"
  val itemDescClass = "Class'/Script/FactoryGame.FGItemDescriptor'"
//  val builders = Set(
//    "/Game/FactoryGame/Equipment/BuildGun/BP_BuildGun.BP_BuildGun_C",
//    "/Script/FactoryGame.FGBuildGun",
//    "/Game/FactoryGame/Buildable/-Shared/WorkBench/BP_WorkshopComponent.BP_WorkshopComponent_C",
//    "/Game/FactoryGame/Buildable/Factory/ConstructorMk1/Build_ConstructorMk1.Build_ConstructorMk1_C",
//    "/Game/FactoryGame/Buildable/-Shared/WorkBench/BP_WorkBenchComponent.BP_WorkBenchComponent_C",
//    "/Script/FactoryGame.FGBuildableAutomatedWorkBench",
//    "/Game/FactoryGame/Buildable/Factory/SmelterMk1/Build_SmelterMk1.Build_SmelterMk1_C",
//    "/Game/FactoryGame/Buildable/Factory/AssemblerMk1/Build_AssemblerMk1.Build_AssemblerMk1_C",
//    "/Game/FactoryGame/Buildable/Factory/AutomatedWorkBench/Build_AutomatedWorkBench.Build_AutomatedWorkBench_C",
//    "/Game/FactoryGame/Buildable/Factory/Converter/Build_Converter.Build_Converter_C",
//    "/Game/FactoryGame/Buildable/Factory/OilRefinery/Build_OilRefinery.Build_OilRefinery_C",
//    "/Game/FactoryGame/Buildable/Factory/FoundryMk1/Build_FoundryMk1.Build_FoundryMk1_C",
//    "/Game/FactoryGame/Buildable/Factory/ManufacturerMk1/Build_ManufacturerMk1.Build_ManufacturerMk1_C"
//  )
  val builders: Set[ClassName] = Set(
    "/Game/FactoryGame/Buildable/Factory/ConstructorMk1/Build_ConstructorMk1.Build_ConstructorMk1_C",
    "/Game/FactoryGame/Buildable/Factory/SmelterMk1/Build_SmelterMk1.Build_SmelterMk1_C",
    "/Game/FactoryGame/Buildable/Factory/AssemblerMk1/Build_AssemblerMk1.Build_AssemblerMk1_C",
    "/Game/FactoryGame/Buildable/Factory/Converter/Build_Converter.Build_Converter_C",
    "/Game/FactoryGame/Buildable/Factory/OilRefinery/Build_OilRefinery.Build_OilRefinery_C",
    "/Game/FactoryGame/Buildable/Factory/FoundryMk1/Build_FoundryMk1.Build_FoundryMk1_C",
    "/Game/FactoryGame/Buildable/Factory/ManufacturerMk1/Build_ManufacturerMk1.Build_ManufacturerMk1_C"
  ).map( ClassName( _ ) )

  def parserDecoder[A]( parser: Parser[A] ): Decoder[A] =
    Decoder[String].emap( Parser.parseOnly( parser, _ ).either )

  val decodeClasses: Decoder[( NativeClass, Vector[Json] )] = for {
    nc      <- Decoder[String].prepare( _.downField( "NativeClass" ) ).map( NativeClass )
    classes <- Decoder[Vector[Json]].prepare( _.downField( "Classes" ) )
  } yield ( nc, classes )

  val decodeRecipeClasses: Decoder[Option[Vector[RecipeJson]]] = Decoder.instance(
    hc =>
      (
        for {
          nc <- OptionT.liftF( hc.get[String]( "NativeClass" ) )
          _  <- OptionT.fromOption[Decoder.Result]( (nc == recipeClass).option( () ) )
          cl <- OptionT.liftF( hc.get[Vector[Json]]( "Classes" ) )
        } yield cl.map( RecipeJson )
      ).value
  )

  def decodeClass[A]( nativeClass: String, decoderOpt: Decoder[Option[A]] ): Decoder[Vector[A]] =
    Decoder.instance(
      hc =>
        for {
          nc  <- hc.get[String]( "NativeClass" )
          _   <- (nc == nativeClass).either( DecodingFailure( "unknown ClassName", Nil ), () )
          cls <- hc.get( "Classes" )( Decoder.decodeVector( decoderOpt ) )
        } yield cls.flatten
    )

  def filterRecipe(
      className: ClassName,
      displayName: String,
      ingredients: NonEmptyList[Countable[ClassName]],
      product: NonEmptyList[Countable[ClassName]],
      producers: NonEmptyList[ClassName]
  ): Option[Recipe[ClassName]] =
    NonEmptyList
      .fromList( producers.filter( builders ) )
      .map( Recipe( className, displayName, ingredients, product, _ ) )

  val decodeRecipe: Decoder[Option[Recipe[ClassName]]] = Decoder
    .instance(
      hc =>
        for {
          cn  <- hc.get[String]( "ClassName" )
          dn  <- hc.get[String]( "mDisplayName" )
          in  <- hc.get( "mIngredients" )( parserDecoder( Parsers.countableList ) )
          out <- hc.get( "mProduct" )( parserDecoder( Parsers.countableList ) )
          pr  <- hc.get( "mProducedIn" )( parserDecoder( Parsers.classNameList ) )
        } yield filterRecipe( ClassName( cn ), dn, in, out, pr )
    )
    .or( none[Recipe[ClassName]].pure[Decoder] )

  val decodeRecipes: Decoder[Vector[Recipe[ClassName]]] = decodeClass( recipeClass, decodeRecipe )

  val decodeItemDesc: Decoder[Option[( ClassName, String )]] = Decoder.instance(
    hc =>
      for {
        cn <- hc.get[String]( "ClassName" )
        dn <- hc.get[String]( "mDisplayName" )
      } yield Some( ( ClassName( cn ), dn ) )
  )

  val decodeItemDescriptions: Decoder[Vector[( ClassName, String )]] = decodeClass( itemDescClass, decodeItemDesc )

  val decodeRecipeData: Decoder[RecipeData[ClassName]] =
    decodeItemDescriptions
      .map( ns => RecipeData( ns.toMap, Vector.empty[Recipe[ClassName]] ) )
      .or( decodeRecipes.map( RecipeData( Map.empty, _ ) ) )
      .or( RecipeData.empty[ClassName].pure[Decoder] )

}
