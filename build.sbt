import sbt._
import sbt.Keys._

// format: off

ThisBuild / organization       := "net.chwthewke"
ThisBuild / scalaOrganization  := "org.scala-lang"
ThisBuild / scalaVersion       := "2.13.8"
// format: on

enablePlugins( FormatPlugin, PlatformDepsPlugin )

val commonSettings = Seq(
  libraryDependencies ++= kindProjector ++ betterMonadicFor,
  conflictManager := ConflictManager.strict,
  updateSbtClassifiers / conflictManager := ConflictManager.default, // this is for IntelliJ
  SettingKey[Seq[String]]( "ide-base-packages" )
    .withRank( KeyRanks.Invisible ) := Seq( "net.chwthewke.satisfactorytools" )
)

val jsResources = TaskKey[Seq[File]]( "js-resources" )

val crossModuleDependencyOverrides: Def.Setting[Seq[ModuleID]] =
  dependencyOverrides ++= Seq(
    "org.typelevel" %%% "cats-core"      % catsVersion,
    "org.typelevel" %%% "alleycats-core" % catsVersion,
    "org.typelevel" %%% "cats-effect"    % catsEffectVersion,
    "io.circe"      %%% "circe-core"     % circeVersion,
    "com.chuusai"   %%% "shapeless"      % "2.3.9"
  )

val jsModuleDependencyOverrides: Def.Setting[Seq[ModuleID]] =
  dependencyOverrides ++=
    Seq(
      "org.scala-js"         %% "scalajs-library"         % "1.10.1",
      "org.scala-js"         %%% "scalajs-dom"            % "2.2.0",
      "com.github.cornerman" %%% "colibri"                % "0.6.0",
      "org.http4s"           %%% "http4s-core"            % http4sVersion,
      "org.http4s"           %%% "http4s-client"          % http4sVersion,
      "org.scodec"           %%% "scodec-bits"            % "1.1.34",
      "co.fs2"               %%% "fs2-core"               % fs2Version,
      "co.fs2"               %%% "fs2-io"                 % fs2Version,
      "org.typelevel"        %%% "cats-effect-kernel"     % catsEffectVersion,
      "org.portable-scala"   %%% "portable-scala-reflect" % "1.1.2"
    )

def addJSCommandAliases( module: String, prefix: String ): Seq[Def.Setting[State => State]] =
  addCommandAlias( s"$prefix-prod", s"$module / fullOptJS / webpack" ) ++
    addCommandAlias( s"$prefix-dev", s"$prefix-devInit; $prefix-devWatchAll; $prefix-devDestroy" ) ++
    addCommandAlias( s"$prefix-devInit", s"; $module / fastOptJS / startWebpackDevServer" ) ++
    addCommandAlias( s"$prefix-devWatchAll", s"~; $module / fastOptJS / webpack" ) ++
    addCommandAlias( s"$prefix-devDestroy", s"$module / fastOptJS / stopWebpackDevServer" )

val core =
  crossProject( JVMPlatform, JSPlatform )
    .crossType( CrossType.Pure )
    .settings( commonSettings )
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
      crossModuleDependencyOverrides
    )
    .jsSettings( jsModuleDependencyOverrides )
    .enablePlugins( SbtBuildInfo, ScalacPlugin )

val `core-jvm` = core.jvm
val `core-js`  = core.js

val solver = project
  .settings( commonSettings )
  .settings( libraryDependencies ++= ojAlgo )
  .dependsOn( `core-jvm` )
  .enablePlugins( ScalacPlugin, DependenciesPlugin )

// Protocol types (e.g. DB IDs) which are common to web-v2 and SPA
val protocol = crossProject( JVMPlatform, JSPlatform )
  .crossType( CrossType.Pure )
  .enablePlugins( ScalacPlugin )
  .settings( commonSettings )
  .settings( crossModuleDependencyOverrides )
  .jsSettings( jsModuleDependencyOverrides )
  .dependsOn( core )

val api = project
  .settings( commonSettings )
  .settings( libraryDependencies ++= catsTime )
  .dependsOn( core.jvm, protocol.jvm )
  .enablePlugins( ScalacPlugin, DependenciesPlugin )

val assets = project
  .settings( commonSettings )
  .settings( libraryDependencies ++= catsEffect ++ pureconfig )
  .dependsOn( `core-jvm` )
  .enablePlugins( ScalacPlugin, DependenciesPlugin )

val persistence = project
  .settings( commonSettings )
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
  .settings( commonSettings )
  .settings( libraryDependencies ++= circeFs2 ++ pureconfigCatsEffect ++ pureconfigFs2 )
  .dependsOn( persistence, assets )
  .enablePlugins( ScalacPlugin, DependenciesPlugin )

val `web-v2` = project
  .settings(
    commonSettings,
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

val `production-calculator` = project
  .settings( commonSettings )
  .settings( mainClass.withRank( KeyRanks.Invisible ) := Some( "net.chwthewke.satisfactory.ProdCalculator" ) )
  .settings( libraryDependencies ++= decline ++ pureconfigCatsEffect )
  .dependsOn( `dev-tools` )
  .enablePlugins( ScalacPlugin, DependenciesPlugin )

val `web-api` = crossProject( JVMPlatform, JSPlatform )
  .crossType( CrossType.Pure )
  .enablePlugins( ScalacPlugin )
  .settings( commonSettings )
  .settings( crossModuleDependencyOverrides )
  .jsSettings( jsModuleDependencyOverrides )
  .settings(
    libraryDependencies ++= Seq(
      "org.http4s" %%% "http4s-circe" % http4sVersion,
      "org.http4s" %%% "http4s-dsl"   % http4sVersion
    )
  )
  .dependsOn( protocol )

val `web-api-jvm` = `web-api`.jvm
val `web-api-js`  = `web-api`.js

val `web-front` = project
  .enablePlugins( ScalacPlugin, ScalaJSPlugin, ScalaJSBundlerPlugin )
  .settings(
    commonSettings,
    libraryDependencies ++= Seq(
      "org.scala-js"         %%% "scala-js-macrotask-executor" % "1.0.0",
      "io.github.outwatch"   %%% "outwatch"                    % "1.0.0-RC8",
      "com.github.cornerman" %%% "colibri"                     % "0.6.0",
      "com.github.cornerman" %%% "colibri-fs2"                 % "0.6.0",
      "org.http4s"           %%% "http4s-dom"                  % "0.2.3"
    ),
    crossModuleDependencyOverrides,
    jsModuleDependencyOverrides,
    // ScalaJS settings
    useYarn := true,
    Compile / npmDevDependencies ++= Seq( "@fun-stack/fun-pack" -> "0.2.0" ),
    scalaJSLinkerConfig ~= (_.withModuleKind( ModuleKind.CommonJSModule ) ),
    scalaJSUseMainModuleInitializer := true,
    webpack / version := "4.46.0",
    startWebpackDevServer / version := "3.11.3",
    webpackDevServerPort := 7284,
    webpackDevServerExtraArgs := Seq( "--color" ),
    fullOptJS / webpackEmitSourceMaps := true,
    fullOptJS / webpackConfigFile := Some( baseDirectory.value / "webpack.config.prod.js" ),
    fastOptJS / webpackBundlingMode := BundlingMode.LibraryOnly(),
    fastOptJS / webpackConfigFile := Some( baseDirectory.value / "webpack.config.dev.js" ),
    Test / requireJsDomEnv := true
    //
  )
  .dependsOn( `web-api-js` )

addJSCommandAliases( module = "web-front", prefix = "web" )

val `web-server` = project
  .enablePlugins( ScalacPlugin, DependenciesPlugin, BuildInfoPlugin )
  .settings(
    libraryDependencies ++=
      http4s ++ http4sBlazeServer ++ http4sCirce ++
        logging ++
        scalatags ++
        pureconfigCatsEffect
  )
  .dependsOn( `web-api-jvm`, persistence, assets )

val `web-server-fast` = project
  .enablePlugins( ScalacPlugin, DependenciesPlugin, BuildInfoPlugin )
  .settings(
    // js interop
    jsResources := {
      val _            = (`web-front` / Compile / fastOptJS / webpack).value
      val targetDir    = (`web-front` / Compile / fastOptJS / crossTarget).value
      val targetDevDir = targetDir / "dev"
      val baseName     = (`web-front` / Compile / fastOptJS / moduleName).value

      Seq(
        targetDevDir / s"$baseName-fastopt-library.js",
        targetDir / s"$baseName-fastopt-loader.js",
        targetDir / s"$baseName-fastopt.js",
        targetDevDir / s"$baseName-fastopt-library.js.map",
        targetDir / s"$baseName-fastopt.js.map"
      )
    },
    Compile / resources ++= jsResources.value,
    buildInfoObject := "WebServerFastBuildInfo",
    buildInfoPackage := "net.chwthewke.satisfactorytools.web.server",
    buildInfoKeys ++= Seq[BuildInfoKey](
      BuildInfoKey.map( jsResources ) { case ( k, files ) => ( k, files.map( _.name ) ) }
    ),
    watchSources ++= (`web-front` / watchSources).value
  )
  .dependsOn( `web-server` )

val `web-server-full` = project
  .enablePlugins( ScalacPlugin, DependenciesPlugin, BuildInfoPlugin )
  .settings(
    // js interop
    jsResources := (`web-front` / Compile / fullOptJS / webpack).value.map( _.data ),
    Compile / resources ++= jsResources.value,
    buildInfoObject := "WebServerFullBuildInfo",
    buildInfoPackage := "net.chwthewke.satisfactorytools.web.server",
    buildInfoKeys ++= Seq[BuildInfoKey](
      BuildInfoKey.map( jsResources ) { case ( k, files ) => ( k, files.map( _.name ) ) }
    ),
    watchSources ++= (`web-front` / watchSources).value
  )
  .dependsOn( `web-server` )

val tests = project
  .settings( commonSettings )
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
    `core-jvm`,
    `core-js`,
    api,
    assets,
    solver,
    persistence,
    `dev-tools`,
    `web-v2`,
    `web-api-jvm`,
    `web-api-js`,
    `web-front`,
    `web-server`,
    `web-server-fast`,
    `web-server-full`,
    `production-calculator`,
    `tests`
  )
