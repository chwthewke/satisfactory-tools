package net.chwthewke.satisfactorytools

import cats.Monoid
import cats.data.NonEmptyVector
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.foldable._
import cats.syntax.option._
import cats.syntax.show._
import io.circe.Json
import io.circe.JsonObject
import net.chwthewke.vendor.io.circe.fs2.byteArrayParser
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

import data.Loader
import model.NativeClass

object ExploreJson extends IOApp {

  def loadJson: IO[Vector[Json]] =
    Loader.io.streamDocsResource
      .through( byteArrayParser )
      .compile
      .toVector

  def printNativeClasses( array: Vector[Json] ): IO[Unit] = {
    val ( notices, nativeClasses ) =
      array
        .flatMap( _.asObject )
        .map(
          obj =>
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
      _.asObject.foldMap(
        obj =>
          obj( "NativeClass" )
            .flatMap( _.asString )
            .map( NativeClass( _ ) )
            .foldMap(
              cls => obj( "Classes" ).flatMap( _.asArray ).foldMap( _.foldMap( _.asObject.foldMap( f( cls ) ) ) )
            )
      )
    )

  def collectNativeClassFields( c: NativeClass ): Vector[Json] => SortedSet[String] =
    exploreNativeClasses( cls => obj => if (cls == c) obj.keys.to( SortedSet ) else SortedSet.empty )

  def collectNativeClassFieldValues(
      c: NativeClass,
      fields: String*
  ): Vector[Json] => SortedMap[String, Vector[Json]] =
    exploreNativeClasses(
      cls =>
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
//      _     <- printNativeClasses( array )
      _ <- IO.println( collectNativeClassFields( NativeClass.resourceExtractorClass )( array ).intercalate( "\n" ) )
      _ <- IO.println(
            collectNativeClassFieldValues( NativeClass.resourceExtractorClass, "mDisplayName" )( array )
              .map( _.show )
              .toVector
              .intercalate( "\n" )
          )
    } yield ExitCode.Success
}
