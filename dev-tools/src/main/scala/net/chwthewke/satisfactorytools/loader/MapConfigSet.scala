package net.chwthewke.satisfactorytools
package loader

import cats.Show
import cats.syntax.all._
import pureconfig.ConfigReader

case class MapConfigSet( configs: Map[Int, MapConfig] )

object MapConfigSet {
  implicit val mapConfigSetShow: Show[MapConfigSet] = cats.derived.semiauto.show[MapConfigSet]
  implicit val mapConfigSetReader: ConfigReader[MapConfigSet] = {
    implicit val versionedMapConfigReader: ConfigReader[( Int, MapConfig )] =
      ConfigReader.fromCursor( cc =>
        (
          cc.fluent.at( "version" ).asInt,
          ConfigReader[MapConfig].from( cc )
        ).tupled
      )

    ConfigReader.fromCursor( cc =>
      cc.fluent
        .at( "configs" )
        .cursor
        .flatMap( ConfigReader[Vector[( Int, MapConfig )]].from( _ ) )
        .map( cfgs => MapConfigSet( cfgs.toMap ) )
    )
  }

}
