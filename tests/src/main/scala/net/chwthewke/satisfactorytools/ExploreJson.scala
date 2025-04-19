package net.chwthewke.satisfactorytools

import cats.Monoid
import cats.data.Ior
import cats.data.NonEmptyVector
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all._
import io.circe.Json
import io.circe.JsonObject
import io.circe.fs2.byteArrayParser
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

import data.NativeClass
import loader.Loader

object ExploreJson extends IOApp {

  def loadJson: IO[Vector[Json]] =
    loadJsonVersion( DataVersionStorage.Release1_0 )

  def loadJsonVersion( version: DataVersionStorage ): IO[Vector[Json]] =
    Loader.io
      .streamDocsResource( version )
      .through( byteArrayParser )
      .compile
      .toVector

  def loadJsonAllVersions: IO[Vector[Json]] =
    DataVersionStorage.values.foldMapM( loadJsonVersion )

  def printSchematicDependenciesCount( array: Vector[Json] ): IO[Unit] = {
    val dbc =
      exploreNativeClasses( nc => obj => Option.when( nc == NativeClass.schematicClass )( obj ).toVector )( array )
        .map( sObj =>
          sObj( "mSchematicDependencies" )
            .flatMap( _.asArray )
            .map( _.size )
            .getOrElse( 0 )
        )
        .groupBy( identity )
        .fmap( _.size )

    IO.println(
      dbc
        .to( SortedMap )
        .map { case ( n, f ) => s"$n: $f" }
        .mkString( "DEPENDENCY BLOCK SIZE FREQ\n", "\n", "" )
    )
  }

  def printLiteralFieldsCsv( nativeClasses: Vector[Json], nativeClass: NativeClass ): IO[Unit] =
    nativeClasses
      .collectFirstSome( extractNativeClass( _ ).filter( _._1 == nativeClass ) )
      .traverse_ { case ( _, classes ) => IO.println( literalFieldsCsv( classes ).mkString_( "\n" ) ) }

  def literalFieldsCsv( classes: Vector[Json] ): NonEmptyVector[String] = {
    import alleycats.std.iterable._
    val fields: SortedSet[String] = classes
      .mapFilter( _.asObject )
      .foldLeft( Map.empty[String, Set[Boolean]] )( ( m, c ) =>
        c.keys
          .mapFilter( k => c( k ).map( v => !v.isArray && !v.isObject ).tupleLeft( k ) )
          .toMap
          .alignWith( m ) {
            case Ior.Left( a )    => Set( a )
            case Ior.Right( b )   => b
            case Ior.Both( a, b ) => b + a
          }
      )
      .flatMap { case ( k, v ) => Option.when( !v.contains( false ) )( k ) }
      .to( SortedSet )

    NonEmptyVector(
      fields.mkString_( "," ),
      classes
        .mapFilter( _.asObject )
        .map( obj =>
          fields
            .to( Iterable )
            .fmap( k => obj( k ).filter( v => !v.isArray && !v.isObject ).fold( "" )( v => v.noSpaces ) )
            .mkString_( "," )
        )
    )
  }

  def printNativeClasses( array: Vector[Json] ): IO[Unit] = {
    val ( notices, nativeClasses ) =
      array
        .flatMap( _.asObject )
        .map( obj =>
          obj( "NativeClass" )
            .flatMap( _.asString )
            .toRight( s"no NativeClass key in ${Json.fromJsonObject( obj ).spaces2}\n" )
        )
        .partitionEither( identity )

    NonEmptyVector
      .fromVector( notices )
      .traverse_( msgs => IO.println( "NOTICE:\n" + msgs.intercalate( "\n" ) ) ) *>
      IO.println( "NativeClasses:\n" + nativeClasses.intercalate( "\n" ) )

  }

  def exploreNativeClasses[A: Monoid]( f: NativeClass => JsonObject => A )( array: Vector[Json] ): A =
    array.foldMap(
      _.asObject.foldMap( obj =>
        obj( "NativeClass" )
          .flatMap( _.as[NativeClass].toOption )
          .foldMap( cls =>
            obj( "Classes" ).flatMap( _.asArray ).foldMap( _.foldMap( _.asObject.foldMap( f( cls ) ) ) )
          )
      )
    )

  def printClassDisplayNames( nativeClassesJson: Vector[Json] ): IO[Unit] =
    IO.println(
      exploreNativeClasses( nc =>
        cls => cls( "mDisplayName" ).flatMap( _.asString ).map( dn => SortedMap( nc -> Vector( dn ) ) )
      )( nativeClassesJson ).orEmpty
        .map {
          case ( nc, names ) =>
            names.mkString( show"$nc\n  ", "\n  ", "" )
        }
        .mkString( "\n\n" )
    )

  def exploreResources( array: Vector[Json] ): Map[String, ( Json, NonEmptyVector[String] )] =
    exploreNativeClasses( nc =>
      obj =>
        Option
          .when( nc == NativeClass.resourceDescClass )( obj )
          .toVector
    )( array )
      .foldMap( obj =>
        obj( "ClassName" )
          .flatMap( _.asString )
          .foldMap( cn =>
            obj.toIterable.toVector.foldMap {
              case ( k, v ) =>
                Map( ( k, Map( ( v, NonEmptyVector.one( cn ) ) ) ) )
            }
          )
      )
      .flatMap {
        case ( key, classesByValue ) =>
          classesByValue.collectFirst {
            case ( v, classes ) if classes.contains_( "Desc_OreCopper_C" ) => ( key, ( v, classes ) )
          }
      }
      .filter( _._2._2.length == 9 )

  def printResources( array: Vector[Json] ): IO[Unit] =
    IO.println(
      exploreResources( array )
        .map {
          case ( field, ( value, classes ) ) =>
            show"""$field
                  |  $value
                  |  ${classes.mkString_( ", " )}
                  |""".stripMargin
        }
        .mkString( "RESOURCES\n", "\n", "\n" )
    )

  private def extractNativeClass( json: Json ): Option[( NativeClass, Vector[Json] )] =
    json.asObject.flatMap( obj =>
      (
        obj( "NativeClass" ).flatMap( _.as[NativeClass].toOption ),
        obj( "Classes" ).flatMap( _.asArray )
      ).tupled
    )

  def showSchemas( array: Vector[Json] ): String =
    array
      .mapFilter( extractNativeClass )
      .map {
        case ( nc, classes ) =>
          show"""$nc
                |
                |${JsonSchema.renderSchemas( JsonSchema.extractSchema( classes ) )}
                |""".stripMargin
      }
      .mkString_( "\n" )

  def collectNativeClassFields( c: NativeClass ): Vector[Json] => SortedSet[String] =
    exploreNativeClasses( cls => obj => if (cls == c) obj.keys.to( SortedSet ) else SortedSet.empty )

  def collectNativeClassFieldValues(
      c: NativeClass,
      fields: String*
  ): Vector[Json] => SortedMap[String, Vector[Json]] =
    exploreNativeClasses( cls =>
      obj =>
        Option
          .when( cls == c )(
            fields.toVector
              .foldMap( f => obj( f ).map( v => ( f, Vector( v ) ) ).to( SortedMap ) )
          )
          .orEmpty
    )

  override def run( args: List[String] ): IO[ExitCode] =
    for {
      array <- loadJson

//      _     <- printClassDisplayNames( array )
//      _     <- printLiteralFieldsCsv( array, NativeClass.colliderClass )
//      _     <- printSchematicDependenciesCount( array )
//      _     <- printResources( array )
//      _     <- IO.println( showSchemas( array ) )
//      _ <- printNativeClasses( array )
      _ <- IO.println( collectNativeClassFields( NativeClass.generatorClass )( array ).intercalate( "\n" ) )
      _ <- IO.println( "" )
//      _ <- IO.println( collectNativeClassFields( NativeClass.recipeClass )( array ).intercalate( "\n" ) )
      _ <- IO.println(
             collectNativeClassFieldValues(
               NativeClass.generatorClass,
               "mFuel",
               "mPowerProduction"
             )( array )
               .map( _.show )
               .toVector
               .intercalate( "\n" )
           )
    } yield ExitCode.Success

}
