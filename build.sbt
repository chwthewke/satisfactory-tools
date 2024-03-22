import sbt._
import sbt.Keys._

// format: off

ThisBuild / organization       := "net.chwthewke"
ThisBuild / scalaOrganization  := "org.scala-lang"
ThisBuild / scalaVersion       := "2.13.10"
// TODO when I can make sense of lm-coursier
ThisBuild / conflictManager                         := ConflictManager.strict
ThisBuild / updateSbtClassifiers / conflictManager  := ConflictManager.default
// format: on

enablePlugins( FormatPlugin, PlatformDepsPlugin )

ThisBuild / SettingKey[Seq[String]]( "ide-base-packages" )
  .withRank( KeyRanks.Invisible ) := Seq( "net.chwthewke.satisfactorytools" )

ThisBuild / Compile / doc / sources                := Seq.empty
ThisBuild / Compile / packageDoc / publishArtifact := false

val compilerPlugins = libraryDependencies ++= kindProjector ++ betterMonadicFor

val core =
  crossProject( JVMPlatform, JSPlatform )
    .crossType( CrossType.Pure )
    .settings( compilerPlugins )
    .settings(
      libraryDependencies ++= Seq(
        "org.typelevel" %%% "cats-core"          % catsVersion,
        "org.typelevel" %%% "cats-free"          % catsVersion,
        "org.typelevel" %%% "cats-effect-kernel" % catsEffectVersion,
        "org.typelevel" %%% "alleycats-core"     % catsVersion,
        "org.typelevel" %%% "kittens"            % "3.0.0",
        "org.typelevel" %%% "mouse"              % "1.2.1",
        "org.tpolecat"  %%% "atto-core"          % "0.9.5",
        "io.circe"      %%% "circe-core"         % circeVersion,
        "io.circe"      %%% "circe-generic"      % circeVersion,
        "io.circe"      %%% "circe-parser"       % circeVersion,
        "com.beachape"  %%% "enumeratum"         % enumeratumVersion,
        "com.beachape"  %%% "enumeratum-cats"    % enumeratumVersion,
        "com.beachape"  %%% "enumeratum-circe"   % enumeratumVersion
      ),
      dependencyOverrides ++= Seq(
        "org.typelevel" %%% "cats-core"      % catsVersion,
        "org.typelevel" %%% "cats-free"      % catsVersion,
        "org.typelevel" %%% "alleycats-core" % catsVersion,
        "io.circe"      %%% "circe-core"     % circeVersion,
        "com.chuusai"   %%% "shapeless"      % "2.3.9"
      )
    )
    .jsSettings(
      dependencyOverrides ++= Seq( "org.scala-js" %% "scalajs-library" % "1.13.1" )
    )
    .enablePlugins( ScalacPlugin )

val solver = project
  .settings( compilerPlugins )
  .settings( libraryDependencies ++= ojAlgo )
  .dependsOn( core.jvm )
  .enablePlugins( ScalacPlugin, DependenciesPlugin )

val api = project
  .settings( compilerPlugins )
  .settings( libraryDependencies ++= catsTime ++ circe )
  .dependsOn( core.jvm )
  .enablePlugins( ScalacPlugin, DependenciesPlugin )

val assets = project
  .settings( compilerPlugins )
  .settings( libraryDependencies ++= catsEffect ++ pureconfig )
  .dependsOn( core.jvm )
  .enablePlugins( ScalacPlugin, DependenciesPlugin )

val persistence = project
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
  .dependsOn( api, solver )
  .enablePlugins( ScalacPlugin, DependenciesPlugin )

val `dev-tools` = project
  .settings( compilerPlugins )
  .settings( libraryDependencies ++= circeFs2 ++ pureconfigCatsEffect ++ pureconfigFs2 )
  .dependsOn( persistence, assets )
  .enablePlugins( ScalacPlugin, DependenciesPlugin )

val `web-v2` = project
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
  .dependsOn( api, persistence )
  .enablePlugins( ScalacPlugin, DependenciesPlugin )

val `web-v2-app` = project
  .dependsOn( `web-v2` )
  .enablePlugins( JavaServerAppPackaging )
  .enablePlugins( LauncherJarPlugin )
  .settings(
    topLevelDirectory := Some( "stw2" ),
    Compile / mainClass := Some( "net.chwthewke.satisfactorytools.web.Main" )
  )

val `production-calculator` = project
  .settings( compilerPlugins )
  .settings( mainClass.withRank( KeyRanks.Invisible ) := Some( "net.chwthewke.satisfactory.ProdCalculator" ) )
  .settings( libraryDependencies ++= decline ++ pureconfigCatsEffect )
  .dependsOn( `dev-tools` )
  .enablePlugins( SbtBuildInfo, ScalacPlugin, DependenciesPlugin )

val `tests` = project
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
    `production-calculator`,
    `persistence`,
    `web-v2`
  )
  .enablePlugins( ScalacPlugin, DependenciesPlugin )

val `satisfactory-tools` = project
  .in( file( "." ) )
  .aggregate(
    core.jvm,
    core.js,
    api,
    assets,
    solver,
    persistence,
    `dev-tools`,
    `web-v2`,
    `web-v2-app`,
    `production-calculator`,
    `tests`
  )
