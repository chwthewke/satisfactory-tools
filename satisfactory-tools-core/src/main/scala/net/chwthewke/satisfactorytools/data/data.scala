package net.chwthewke.satisfactorytools

import pureconfig.ConfigReader

package object data {
  implicit def classNameMapReader[A](
      implicit reader: ConfigReader[Map[String, A]]
  ): ConfigReader[Map[ClassName, A]] =
    reader.map( _.map { case ( cn, a ) => ( ClassName( cn ), a ) } )

  private[data] implicit def nativeClassMapReader[A](
      implicit reader: ConfigReader[Map[String, A]]
  ): ConfigReader[Map[NativeClass, A]] =
    reader.map( _.map { case ( cn, a ) => ( NativeClass( cn ), a ) } )
}
