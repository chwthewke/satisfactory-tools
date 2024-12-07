import sbt._
import sbt.Keys._

// format: off

ThisBuild / organization       := "net.chwthewke"
ThisBuild / scalaOrganization  := "org.scala-lang"
ThisBuild / scalaVersion       := "2.13.15"
// TODO when I can make sense of lm-coursier
ThisBuild / conflictManager                         := ConflictManager.strict
ThisBuild / updateSbtClassifiers / conflictManager  := ConflictManager.default
// format: on

enablePlugins( FormatPlugin )

ThisBuild / SettingKey[Seq[String]]( "ide-base-packages" )
  .withRank( KeyRanks.Invisible ) := Seq( "net.chwthewke.satisfactorytools" )

ThisBuild / Compile / doc / sources                := Seq.empty
ThisBuild / Compile / packageDoc / publishArtifact := false

val compilerPlugins = libraryDependencies ++= kindProjector ++ betterMonadicFor

val core =
  project
    .settings( compilerPlugins )
    .settings(
      libraryDependencies ++=
        cats ++ catsFree ++ alleycatsCore ++ kittens ++ mouse ++
          atto ++ circe ++ enumeratum ++ enumeratumCirce ++ algebra
    )
    .enablePlugins( ScalacPlugin )

val solver = project
  .settings( compilerPlugins )
  .settings( libraryDependencies ++= ojAlgo )
  .dependsOn( core )
  .enablePlugins( ScalacPlugin, DependenciesPlugin )

val api = project
  .settings( compilerPlugins )
  .settings( libraryDependencies ++= catsTime ++ circe )
  .dependsOn( core )
  .enablePlugins( ScalacPlugin, DependenciesPlugin )

val assets = project
  .settings( compilerPlugins )
  .settings( libraryDependencies ++= catsEffect ++ pureconfig )
  .dependsOn( core )
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
    topLevelDirectory    := Some( "stw2" ),
    Compile / mainClass  := Some( "net.chwthewke.satisfactorytools.web.Main" ),
    Compile / run / fork := true
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
    core,
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
