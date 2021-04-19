import sbt._
import sbt.Keys._

// format: off
organization      in ThisBuild := "net.chwthewke"
scalaOrganization in ThisBuild := "org.scala-lang"
scalaVersion      in ThisBuild := "2.13.5"
// TODO when I can make sense of lm-coursier
conflictManager   in ThisBuild                         := ConflictManager.strict
conflictManager   in updateSbtClassifiers in ThisBuild := ConflictManager.default
// format: on

enablePlugins( FormatPlugin, DependenciesPlugin )

SettingKey[Seq[String]]( "ide-base-packages" ) in ThisBuild := Seq( "net.chwthewke.satisfactorytools" )

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
        fs2 ++
        circeFs2 ++
        decline
  )
  .dependsOn( `satisfactory-tools-core` )
  .enablePlugins( ScalacPlugin )

val `satisfactory-tools-dev` = project
  .settings( compilerPlugins )
  .dependsOn( `satisfactory-tools-app` )
  .enablePlugins( ScalacPlugin )

val `satisfactory-production-calculator` = project
  .settings( compilerPlugins )
  .settings( mainClass := Some( "net.chwthewke.satisfactory.ProdCalculator" ) )
  .dependsOn( `satisfactory-tools-app` )
  .enablePlugins( ScalacPlugin )

val `satisfactory-tools-tests` = project
  .settings( compilerPlugins )
  .settings( libraryDependencies ++= breeze ++ (scalatest ++ scalacheck).map( _ % "test" ) )
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
  .dependsOn( `satisfactory-tools-app` )
  .enablePlugins( ScalacPlugin )

val `satisfactory-tools-all` = project
  .in( file( "." ) )
  .aggregate(
    `satisfactory-tools-core`,
    `satisfactory-tools-tests`
  )
