package net.chwthewke.satisfactorytools

import cats.Monoid
import cats.data.NonEmptyVector
import cats.effect.Blocker
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.foldable._
import io.circe.Json
import io.circe.JsonObject
import io.circe.fs2.byteArrayParser
import scala.collection.immutable.SortedSet
//
import load.Loader
import model.NativeClass

object ExploreRaw extends IOApp {

  def loadJson( blocker: Blocker ): IO[Vector[Json]] =
    Loader.io
      .streamDocsResource( blocker )
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
      .traverse_( msgs => IO.delay( println( "NOTICE:\n" + msgs.intercalate( "\n" ) ) ) ) *>
      IO.delay(
        println(
          "NativeClasses:\n" + nativeClasses.intercalate( "\n" )
        )
      )

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

  override def run( args: List[String] ): IO[ExitCode] =
    for {
      array <- Blocker[IO].use( loadJson )
//      _     <- printNativeClasses( array )
      _ <- IO.delay(
            println(
              collectNativeClassFields( NativeClass( "Class'/Script/FactoryGame.FGSchematic'" ) )( array )
                .intercalate( "\n" )
            )
          )
    } yield ExitCode.Success
}
