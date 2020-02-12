name := "satisfactory-tools-core-build"

resolvers += Classpaths.sbtPluginReleases
resolvers += Resolver.sonatypeRepo( "releases" )

addSbtPlugin( "org.scoverage"    % "sbt-scoverage"       % "1.6.0" )
addSbtPlugin( "com.eed3si9n"     % "sbt-buildinfo"       % "0.9.0" )
addSbtPlugin( "com.typesafe.sbt" % "sbt-native-packager" % "1.4.1" )
addSbtPlugin( "org.scalameta"    % "sbt-scalafmt"        % "2.0.7" )
