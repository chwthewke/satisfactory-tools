package net.chwthewke.satisfactorytools

import pureconfig.ConfigReader

import data.ClassName

package object loader {
  implicit def classNameMapReader[A]( implicit
      reader: ConfigReader[Map[String, A]]
  ): ConfigReader[Map[ClassName, A]] =
    reader.map( _.map { case ( cn, a ) => ( ClassName( cn ), a ) } )
}
