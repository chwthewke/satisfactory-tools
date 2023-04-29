name := "satisfactory-tools-core-build"

resolvers += Classpaths.sbtPluginReleases
resolvers += Resolver.sonatypeRepo( "releases" )

addSbtPlugin( "org.scoverage"  % "sbt-scoverage"       % "2.0.7" )
addSbtPlugin( "com.eed3si9n"   % "sbt-buildinfo"       % "0.11.0" )
addSbtPlugin( "com.github.sbt" % "sbt-native-packager" % "1.9.16" )
addSbtPlugin( "org.scalameta"  % "sbt-scalafmt"        % "2.5.0" )
//
addSbtPlugin( "org.scala-js"       % "sbt-scalajs"              % "1.13.1" )
addSbtPlugin( "org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.1" )
