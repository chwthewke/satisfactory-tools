package net.chwthewke.satisfactorytools
package assets

import cats.Order.catsKernelOrderingForOrder
import cats.Show
import cats.syntax.foldable._
import pureconfig.ConfigReader
import pureconfig.ConfigWriter
import pureconfig.generic.semiauto

/**
  * Given a texture name and a version, get the path to the icon
  */
case class IconIndex( icons: Map[( String, Int ), String] ) {
  def getIconPath( version: Int, textureName: String ): Option[String] =
    icons.get( ( textureName, version ) )
}

object IconIndex {
  implicit val iconIndexShow: Show[IconIndex] = Show.show(
    index =>
      s"""IconIndex(
         |  ${index.icons.toVector
           .sortBy { case ( ( n, v ), _ ) => ( v, n ) }
           .map { case ( ( n, v ), p ) => s"$v/${n.padTo( 40, ' ' )} => $p" }
           .mkString( "\n" )}
         |)""".stripMargin
  )

  private case class IconConfig( textureName: String, iconPath: String )

  private implicit val iconConfigWriter: ConfigWriter[IconConfig] = semiauto.deriveWriter[IconConfig]
  private implicit val iconConfigReader: ConfigReader[IconConfig] = semiauto.deriveReader[IconConfig]

  private case class VersionConfig( version: Int, icons: Vector[IconConfig] )

  private implicit val versionConfigWriter: ConfigWriter[VersionConfig] = semiauto.deriveWriter[VersionConfig]
  private implicit val versionConfigReader: ConfigReader[VersionConfig] = semiauto.deriveReader[VersionConfig]

  private case class VersionConfigSet( configs: Vector[VersionConfig] )

  private implicit val versionConfigSetWriter: ConfigWriter[VersionConfigSet] = semiauto.deriveWriter[VersionConfigSet]
  private implicit val versionConfigSetReader: ConfigReader[VersionConfigSet] = semiauto.deriveReader[VersionConfigSet]

  implicit def iconIndexConfigWriter: ConfigWriter[IconIndex] =
    ConfigWriter[VersionConfigSet]
      .contramap(
        iconIndex =>
          VersionConfigSet(
            iconIndex.icons.toVector
              .groupMap { case ( ( _, v ), _ ) => v } { case ( ( n, _ ), p ) => IconConfig( n, p ) }
              .toVector
              .map { case ( v, nps ) => VersionConfig( v, nps.sortBy( _.textureName ) ) }
              .sortBy( _.version )
          )
      )

  implicit def iconIndexConfigReader: ConfigReader[IconIndex] =
    ConfigReader[VersionConfigSet]
      .map(
        configSet =>
          IconIndex( configSet.configs.foldMap {
            case VersionConfig( version, icons ) =>
              icons.foldMap {
                case IconConfig( textureName, iconPath ) =>
                  Map( ( textureName, version ) -> iconPath )
              }
          } )
      )

}
