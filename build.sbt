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

enablePlugins( FormatPlugin, DependenciesPlugin )

ThisBuild / SettingKey[Seq[String]]( "ide-base-packages" )
  .withRank( KeyRanks.Invisible ) := Seq( "net.chwthewke.dsptools", "net.chwthewke.satisfactorytools" )

val compilerPlugins = libraryDependencies ++= kindProjector ++ betterMonadicFor

val testDependencies =
  autoDiff ++
    autoDiffEnumeratum ++
    autoDiffScalatest ++
    scalatest ++
    scalacheck ++
    discipline ++
    catsLaws

val noPublish: Seq[Def.Setting[_]] = Seq(
  publish := {},
  publishLocal := {},
  publishArtifact := false
)

val `satisfactory-tools-core` = project
  .settings( compilerPlugins )
  .settings(
    libraryDependencies ++=
      cats ++
        alleycatsCore ++
        mouse ++
        kittens ++
        catsTime ++
        atto ++
        circe ++
        enumeratum ++
        enumeratumCirce ++
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

val `satisfactory-tools-api` = project
  .settings( compilerPlugins )
  .settings(
    libraryDependencies ++= circe
  )
  .dependsOn( `satisfactory-tools-core` )
  .enablePlugins( ScalacPlugin )

val `satisfactory-tools-persistence` = project
  .settings( compilerPlugins )
  .settings(
    libraryDependencies ++=
      doobie ++
        doobiePostgres ++
        doobiePostgresCirce ++
        flywayCore ++
        postgresql ++
        http4s ++
        http4sBlazeServer ++
        logging
  )
  .dependsOn( `satisfactory-tools-app`, `satisfactory-tools-api` )
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
  .dependsOn( `satisfactory-tools-app`, `satisfactory-tools-api` )
  .enablePlugins( ScalacPlugin )

val `satisfactory-tools-web-v2` = project
  .settings( compilerPlugins )
  .settings(
    libraryDependencies ++=
      http4s ++
        http4sBlazeServer ++
        scodec ++
        scalatags ++
        logging
  )
  .dependsOn( `satisfactory-tools-api`, `satisfactory-tools-persistence` )
  .enablePlugins( ScalacPlugin )

val `satisfactory-tools-dev` = project
  .settings( compilerPlugins )
  .dependsOn( `satisfactory-tools-app` )
  .enablePlugins( ScalacPlugin )

val `satisfactory-production-calculator` = project
  .settings( compilerPlugins )
  .settings( mainClass.withRank( KeyRanks.Invisible ) := Some( "net.chwthewke.satisfactory.ProdCalculator" ) )
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
        catsLaws ++
        doobieScalatest
    ).map( _ % "test" )
  )
  .settings( Test / fork := true )
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
    `satisfactory-tools-web`,
    `satisfactory-tools-web-v2`
  )
  .enablePlugins( ScalacPlugin )

val `factory-tools` = project
  .in( file( "common/factory-tools" ) )
  .settings( compilerPlugins )
  .settings(
    libraryDependencies ++=
      cats ++
        alleycatsCore ++
        mouse ++
        kittens ++
        catsTime ++
        enumeratum ++
        ojAlgo
  )
  .enablePlugins( ScalacPlugin )

val `factory-tools-tests` = project
  .in( file( "common/factoy-tools-test" ) )
  .settings( compilerPlugins )
  .settings( libraryDependencies ++= testDependencies )
  .dependsOn( `factory-tools` )
  .enablePlugins( ScalacPlugin )

val `factory-tools-all` = project
  .in( file( "common" ) )
  .aggregate(
    `factory-tools`,
    `factory-tools-tests`
  )
  .settings( noPublish )

val `dsp-tools-core` = project
  .in( file( "dsp/dsp-tools-core" ) )
  .settings( compilerPlugins )
  .settings(
    libraryDependencies ++= cats ++ kittens ++ alleycatsCore ++ mouse ++ enumeratum ++ scodec ++ spire
  )
  .dependsOn( `factory-tools` )
  .enablePlugins( ScalacPlugin )

val `dsp-tools-app` = project
  .in( file( "dsp/dsp-tools-app" ) )
  .settings( compilerPlugins )
  .settings( libraryDependencies ++= catsEffect ++ fs2 )
  .dependsOn( `dsp-tools-core` )
  .enablePlugins( ScalacPlugin )

val `dsp-tools-dev` = project
  .in( file( "dsp/dsp-tools-dev" ) )
  .settings( compilerPlugins )
  .settings( libraryDependencies ++= fs2Scodec )
  .dependsOn( `dsp-tools-app` )
  .enablePlugins( ScalacPlugin )

val `dsp-tools-cli` = project
  .in( file( "dsp/dsp-tools-cli" ) )
  .settings( compilerPlugins )
  .settings( libraryDependencies ++= pureconfig ++ pureconfigCatsEffect )
  .dependsOn( `dsp-tools-app` )
  .enablePlugins( ScalacPlugin )

val `dsp-tools-tests` = project
  .in( file( "dsp/dsp-tools-tests" ) )
  .settings( compilerPlugins )
  .settings( libraryDependencies ++= testDependencies )
  .dependsOn( `dsp-tools-dev`, `dsp-tools-cli` )
  .enablePlugins( ScalacPlugin )

val `dsp-tools-all` = project
  .in( file( "dsp" ) )
  .aggregate(
    `dsp-tools-core`,
    `dsp-tools-app`,
    `dsp-tools-cli`,
    `dsp-tools-dev`,
    `dsp-tools-tests`
  )

val `satisfactory-tools-all` = project
  .in( file( "." ) )
  .aggregate(
    `factory-tools-all`,
    `satisfactory-tools-core`,
    `satisfactory-tools-app`,
    `satisfactory-tools-api`,
    `satisfactory-tools-persistence`,
    `satisfactory-tools-dev`,
    `satisfactory-tools-web`,
    `satisfactory-tools-web-v2`,
    `satisfactory-production-calculator`,
    `satisfactory-tools-tests`,
    `dsp-tools-all`
  )
