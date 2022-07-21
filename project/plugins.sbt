name := "satisfactory-tools-core-build"

resolvers += Classpaths.sbtPluginReleases
resolvers += Resolver.sonatypeRepo( "releases" )

addSbtPlugin( "org.scoverage"    % "sbt-scoverage"       % "1.6.0" )
addSbtPlugin( "com.eed3si9n"     % "sbt-buildinfo"       % "0.9.0" )
addSbtPlugin( "com.typesafe.sbt" % "sbt-native-packager" % "1.4.1" )
addSbtPlugin( "org.scalameta"    % "sbt-scalafmt"        % "2.0.7" )
//
addSbtPlugin( "org.scala-js"       % "sbt-scalajs"              % "1.10.0" )
addSbtPlugin( "org.portable-scala" % "sbt-scalajs-crossproject" % "1.2.0" )
addSbtPlugin( "ch.epfl.scala"      % "sbt-scalajs-bundler"      % "0.20.0" )
