package net.chwthewke.satisfactorytools

import com.monovore.decline.Opts
import pureconfig.ConfigSource

object Program {
  def configOpt: Opts[ConfigSource] =
    Opts
      .argument[String]( "CONFIG" )
      .orNone
      .map(
        _.fold( ConfigSource.default )(
          name =>
            ConfigSource
              .resources( s"$name.conf" )
              .withFallback( ConfigSource.defaultReference )
        )
      )
}
