import sbt._
import sbt.Keys._

// format: off

ThisBuild / organization       := "net.chwthewke"
ThisBuild / scalaOrganization  := "org.scala-lang"
ThisBuild / scalaVersion       := "2.13.5"
// TODO when I can make sense of lm-coursier
ThisBuild / conflictManager                         := ConflictManager.strict
ThisBuild / updateSbtClassifiers / conflictManager  := ConflictManager.default
// format: on

enablePlugins( FormatPlugin, DependenciesPlugin )

ThisBuild / SettingKey[Seq[String]]( "ide-base-packages" ) := Seq( "net.chwthewke.satisfactorytools" )

val compilerPlugins = libraryDependencies ++= kindProjector ++ splain ++ betterMonadicFor

val `satisfactory-tools-core` = project
  .settings( compilerPlugins )
  .settings(
    libraryDependencies ++=
      cats ++
        alleycatsCore ++
        mouse ++
        kittens ++
        atto ++
        circe ++
        enumeratum ++
        enumeratumCirce ++
        asciiGraphs ++
        graphs ++
        ojAlgo ++
        pureconfig
  )
  .enablePlugins( SbtBuildInfo, ScalacPlugin )

val `satisfactory-tools-app` = project
  .settings( compilerPlugins )
  .settings(
    libraryDependencies ++=
      catsEffect ++
        pureconfigCatsEffect ++
        fs2 // ++ circeFs2
  )
  .dependsOn( `satisfactory-tools-core` )
  .enablePlugins( ScalacPlugin )

val `satisfactory-tools-web` = project
  .settings( compilerPlugins )
  .settings(
    libraryDependencies ++=
      http4s ++
        http4sBlazeServer ++
        scodec ++
        scalatags ++
        logging
  )
  .dependsOn( `satisfactory-tools-app` )
  .enablePlugins( ScalacPlugin )

val `satisfactory-tools-dev` = project
  .settings( compilerPlugins )
  .dependsOn( `satisfactory-tools-app` )
  .enablePlugins( ScalacPlugin )

val `satisfactory-production-calculator` = project
  .settings( compilerPlugins )
  .settings( mainClass := Some( "net.chwthewke.satisfactory.ProdCalculator" ) )
  .settings( libraryDependencies ++= decline )
  .dependsOn( `satisfactory-tools-app` )
  .enablePlugins( ScalacPlugin )

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
        catsLaws
    ).map( _ % "test" )
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
  .dependsOn( `satisfactory-production-calculator`, `satisfactory-tools-web` )
  .enablePlugins( ScalacPlugin )

val `satisfactory-tools-all` = project
  .in( file( "." ) )
  .aggregate(
    `satisfactory-tools-core`,
    `satisfactory-tools-app`,
    `satisfactory-tools-dev`,
    `satisfactory-tools-web`,
    `satisfactory-production-calculator`,
    `satisfactory-tools-tests`
  )
