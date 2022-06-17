package net.chwthewke.satisfactorytools
package model

import cats.Show
import cats.syntax.apply._
import cats.syntax.functor._
import cats.syntax.traverse._
import pureconfig.ConfigReader
import pureconfig.error.CannotConvert
import pureconfig.generic.semiauto

import data.ClassName

case class MapConfig( resourceNodes: Map[ExtractorType, Map[ClassName, ResourceDistrib]] )

object MapConfig {
  private implicit val resourceDistribReader: ConfigReader[ResourceDistrib] =
    ConfigReader[Vector[Int]]
      .emap(
        counts =>
          ( counts.lift( 0 ), counts.lift( 1 ), counts.lift( 2 ) )
            .mapN( ResourceDistrib( _, _, _ ) )
            .toRight(
              CannotConvert( counts.mkString( "[", ", ", "]" ), "ResourceDistrib", "the array must have 3 elements" )
            )
      )

  private implicit def extractorTypeMapReader[A](
      implicit reader: ConfigReader[Map[String, A]]
  ): ConfigReader[Map[ExtractorType, A]] =
    reader.emap(
      _.toVector
        .traverse {
          case ( k, v ) =>
            ExtractorType
              .withNameOption( k )
              .toRight( CannotConvert( k, "ExtractorType", "Unknown key" ) )
              .tupleRight( v )
        }
        .map( _.toMap )
    )

  implicit val mapConfigReader: ConfigReader[MapConfig] =
    semiauto.deriveReader[MapConfig]

  implicit val mapConfigShow: Show[MapConfig] = cats.derived.semiauto.show[MapConfig]

}

case class MapConfigSet( configs: Map[Int, MapConfig] )

object MapConfigSet {
  implicit val mapConfigSetShow: Show[MapConfigSet] = cats.derived.semiauto.show[MapConfigSet]
  implicit val mapConfigSetReader: ConfigReader[MapConfigSet] = {
    implicit val versionedMapConfigReader: ConfigReader[( Int, MapConfig )] =
      ConfigReader.fromCursor(
        cc =>
          (
            cc.fluent.at( "version" ).asInt,
            ConfigReader[MapConfig].from( cc )
          ).tupled
      )

    ConfigReader.fromCursor(
      cc =>
        cc.fluent
          .at( "configs" )
          .cursor
          .flatMap( ConfigReader[Vector[( Int, MapConfig )]].from( _ ) )
          .map( cfgs => MapConfigSet( cfgs.toMap ) )
    )
  }

}
