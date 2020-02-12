import sbt._
import sbt.Keys._

// format: off
organization      in ThisBuild := "net.chwthewke"
scalaOrganization in ThisBuild := "org.scala-lang"
scalaVersion      in ThisBuild := "2.13.1"
// TODO when I can make sense of lm-coursier
conflictManager   in ThisBuild                         := ConflictManager.strict
conflictManager   in updateSbtClassifiers in ThisBuild := ConflictManager.default
// format: on

enablePlugins( FormatPlugin, DependenciesPlugin )

val `satisfactory-tools-core` = project
  .settings( libraryDependencies ++= cats ++ catsEffect )
  .enablePlugins( SbtBuildInfo, ScalacPlugin )

val `satisfactory-tools-tests` = project
  .settings( libraryDependencies ++= (scalatest ++ scalacheck).map( _ % "test" ) )
  .dependsOn( `satisfactory-tools-core` )
  .enablePlugins( ScalacPlugin )

val `satisfactory-tools-all` = project
  .in( file( "." ) )
  .aggregate(
    `satisfactory-tools-core`,
    `satisfactory-tools-tests`
  )
