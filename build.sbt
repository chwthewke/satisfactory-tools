import sbt._
import sbt.Keys._

// format: off

ThisBuild / organization       := "net.chwthewke"
ThisBuild / scalaOrganization  := "org.scala-lang"
ThisBuild / scalaVersion       := "2.13.8"
// TODO when I can make sense of lm-coursier
ThisBuild / conflictManager                         := ConflictManager.strict
ThisBuild / updateSbtClassifiers / conflictManager  := ConflictManager.default
// format: on

enablePlugins( FormatPlugin, PlatformDepsPlugin )

ThisBuild / SettingKey[Seq[String]]( "ide-base-packages" )
  .withRank( KeyRanks.Invisible ) := Seq( "net.chwthewke.satisfactorytools" )

val compilerPlugins = libraryDependencies ++= kindProjector ++ betterMonadicFor

val `satisfactory-tools-core` =
  crossProject( JVMPlatform, JSPlatform )
    .crossType( CrossType.Pure )
    .settings( compilerPlugins )
    .settings(
      libraryDependencies ++= Seq(
        "org.typelevel" %%% "cats-core"        % catsVersion,
        "org.typelevel" %%% "alleycats-core"   % catsVersion,
        "org.typelevel" %%% "kittens"          % "2.3.2",
        "org.typelevel" %%% "mouse"            % "1.1.0",
        "org.tpolecat"  %%% "atto-core"        % "0.9.5",
        "io.circe"      %%% "circe-core"       % circeVersion,
        "io.circe"      %%% "circe-generic"    % circeVersion,
        "io.circe"      %%% "circe-parser"     % circeVersion,
        "com.beachape"  %%% "enumeratum"       % enumeratumVersion,
        "com.beachape"  %%% "enumeratum-cats"  % enumeratumVersion,
        "com.beachape"  %%% "enumeratum-circe" % enumeratumVersion
      ),
      dependencyOverrides ++= Seq(
        "org.typelevel" %%% "cats-core"      % catsVersion,
        "org.typelevel" %%% "alleycats-core" % catsVersion,
        "io.circe"      %%% "circe-core"     % circeVersion,
        "com.chuusai"   %%% "shapeless"      % "2.3.9"
      )
    )
    .jsSettings(
      dependencyOverrides ++= Seq( "org.scala-js" %% "scalajs-library" % "1.10.1" )
    )
    .enablePlugins( SbtBuildInfo, ScalacPlugin )

val `satisfactory-tools-solver` = project
  .settings( compilerPlugins )
  .settings( libraryDependencies ++= ojAlgo )
  .dependsOn( `satisfactory-tools-core`.jvm )
  .enablePlugins( ScalacPlugin, DependenciesPlugin )

val `satisfactory-tools-api` = project
  .settings( compilerPlugins )
  .settings(
    libraryDependencies ++=
      catsTime ++
        circe
  )
  .dependsOn( `satisfactory-tools-core`.jvm )
  .enablePlugins( ScalacPlugin, DependenciesPlugin )

val `satisfactory-tools-persistence` = project
  .settings( compilerPlugins )
  .settings(
    libraryDependencies ++=
      doobie ++
        doobiePostgres ++
        doobiePostgresCirce ++
        flywayCore ++
        postgresql ++
        pureconfig ++
        http4s ++
        http4sBlazeServer ++
        logging
  )
  .dependsOn( `satisfactory-tools-api`, `satisfactory-tools-solver` )
  .enablePlugins( ScalacPlugin, DependenciesPlugin )

val `satisfactory-tools-dev` = project
  .settings( compilerPlugins )
  .settings( libraryDependencies ++= circeFs2 ++ pureconfigCatsEffect )
  .dependsOn( `satisfactory-tools-persistence` )
  .enablePlugins( ScalacPlugin, DependenciesPlugin )

val `satisfactory-tools-web-v2` = project
  .settings(
    compilerPlugins,
    Compile / run / fork := true,
    libraryDependencies ++=
      http4s ++
        http4sBlazeServer ++
        scodec ++
        scalatags ++
        logging ++
        pureconfigCatsEffect
  )
  .dependsOn( `satisfactory-tools-api`, `satisfactory-tools-persistence` )
  .enablePlugins( ScalacPlugin, DependenciesPlugin )

val `satisfactory-production-calculator` = project
  .settings( compilerPlugins )
  .settings( mainClass.withRank( KeyRanks.Invisible ) := Some( "net.chwthewke.satisfactory.ProdCalculator" ) )
  .settings( libraryDependencies ++= decline ++ pureconfigCatsEffect )
  .dependsOn( `satisfactory-tools-dev` )
  .enablePlugins( ScalacPlugin, DependenciesPlugin )

val `satisfactory-tools-tests` = project
  .settings( compilerPlugins )
  .settings(
    libraryDependencies ++= (
      autoDiff ++
        autoDiffEnumeratum ++
        autoDiffScalatest ++
        scalatest ++
        scalacheck ++
        discipline ++
        catsLaws ++
        doobieScalatest
    ).map( _ % "test" )
  )
  .settings(
    Test / fork := true,
    Test / testOptions += Tests.Argument( TestFrameworks.ScalaTest, "-oDF" )
  )
  .settings(
    initialCommands := Seq(
      "import cats._",
      "import cats.implicits._",
      "import cats.effect._",
      "import fs2.Stream",
      "import io.circe._",
      "import net.chwthewke.satisfactorytools._"
    ).mkString( "\n" )
  )
  .dependsOn(
    `satisfactory-production-calculator`,
    `satisfactory-tools-persistence`,
    `satisfactory-tools-web-v2`
  )
  .enablePlugins( ScalacPlugin, DependenciesPlugin )

val `satisfactory-tools-all` = project
  .in( file( "." ) )
  .aggregate(
    `satisfactory-tools-core`.jvm,
    `satisfactory-tools-core`.js,
    `satisfactory-tools-api`,
    `satisfactory-tools-solver`,
    `satisfactory-tools-persistence`,
    `satisfactory-tools-dev`,
    `satisfactory-tools-web-v2`,
    `satisfactory-production-calculator`,
    `satisfactory-tools-tests`
  )
