import sbt.Def
import sbt._
import sbt.Keys._
import sbt.librarymanagement.DependencyBuilders
import scala.language.implicitConversions

object DependenciesPlugin extends AutoPlugin {
  type Deps = Seq[ModuleID]

  object autoImport {
    type Deps = DependenciesPlugin.Deps

    implicit def ToStringOps( orgName: String ): StringOps = new StringOps( orgName )

    implicit def ToDbOanOps( dbOans: Seq[DependencyBuilders.OrganizationArtifactName] ): DbOanOps =
      new DbOanOps( dbOans )

    implicit def ToGroupOps( deps: Deps ): GroupOps = new GroupOps( deps )

    val kindProjector: Deps =
      Seq( compilerPlugin( "org.typelevel" %% "kind-projector" % "0.13.3" cross CrossVersion.full ) )

    val betterMonadicFor: Deps = Seq( compilerPlugin( "com.olegpy" %% "better-monadic-for" % "0.3.1" ) )

    val catsVersion         = "2.12.0"
    val cats: Deps          = "org.typelevel" %% Seq( "cats-core", "cats-kernel" ) % catsVersion
    val catsFree: Deps      = Seq( "org.typelevel" %% "cats-free" % catsVersion )
    val catsMtl: Deps       = Seq( "org.typelevel" %% "cats-mtl-core" % "0.7.1" )
    val mouse: Deps         = Seq( "org.typelevel" %% "mouse" % "1.3.2" )
    val kittens: Deps       = Seq( "org.typelevel" %% "kittens" % "3.4.0" )
    val alleycatsCore: Deps = Seq( "org.typelevel" %% "alleycats-core" % catsVersion )
    val catsTime: Deps      = Seq( "org.typelevel" %% "cats-time" % "0.5.1" )

    val catsEffect: Deps = "org.typelevel" %% Seq( "cats-effect", "cats-effect-kernel", "cats-effect-std" ) % "3.5.4"

    val fs2: Deps = "co.fs2" %% Seq( "fs2-core", "fs2-io" ) % "3.10.2"

    val http4s: Deps            = "org.http4s" %% Seq( "http4s-core", "http4s-dsl" ) % "0.23.28"
    val http4sBlazeServer: Deps = Seq( "org.http4s" %% "http4s-blaze-server" % "0.23.16" )
    val http4sBlazeClient: Deps = Seq( "org.http4s" %% "http4s-blaze-client" % "0.23.16" )

    val http4sPinnedDependencies: Deps = Seq(
      "com.comcast" %% "ip4s-core" % "3.6.0"
//      "org.typelevel" %% "literally" % "1.0.1"
    )

    val scalatags: Deps = Seq( "com.lihaoyi" %% "scalatags" % "0.13.1" )

    val monocleVersion = "3.2.0"
    val monocle: Deps  = "dev.optics" %% Seq( "monocle-core", "monocle-macro" ) % monocleVersion

    val circeVersion = "0.14.10"
    val circe: Deps  = "io.circe" %% Seq( "circe-core", "circe-generic", "circe-parser" ) % circeVersion
//    val circeOptics: Deps = Seq( "io.circe" %% "circe-optics" % "0.14.1" )
    val circeFs2: Deps                = Seq( "io.circe" %% "circe-fs2" % "0.14.1" )
    val circeJawn: Deps               = Seq( "io.circe" %% "circe-jawn" % circeVersion )
    val circePinnedDependencies: Deps = Seq( "org.typelevel" %% "jawn-parser" % "1.6.0" )

    val scodec: Deps = Seq(
      "org.scodec" %% "scodec-bits" % "1.2.1",
      "org.scodec" %% "scodec-core" % "1.11.10"
    )

    val atto: Deps = Seq( "org.tpolecat" %% "atto-core" % "0.9.5" )

    val asciiGraphs: Deps = Seq( "org.scalameta" %% "ascii-graphs" % "0.1.2" )
    val graphs: Deps      = "com.flowtick" %% Seq( "graphs-core", "graphs-cats" ) % "0.5.0"

    val spire: Deps    = Seq( "org.typelevel" %% "spire" % "0.17.0-M1" )
    val algebird: Deps = Seq( "com.twitter" %% "algebird-core" % "0.13.6" )
    val algebra: Deps  = Seq( "org.typelevel" %% "algebra" % "2.0.0" )

    val breeze: Deps = "org.scalanlp" %% Seq( "breeze", "breeze-natives" ) % "1.1"
    val breezePinnedDependencies: Deps =
      Seq(
        "org.apache.commons"       % "commons-math3" % "3.5",
        "com.github.fommil.netlib" % "core"          % "1.1.2"
      )

    val ojAlgo: Deps = Seq( "org.ojalgo" % "ojalgo" % "55.0.0" )

    val enumeratumVersion: String = "1.7.4"
    val enumeratum: Deps          = "com.beachape" %% Seq( "enumeratum", "enumeratum-cats" ) % enumeratumVersion
    val enumeratumCirce: Deps     = Seq( "com.beachape" %% "enumeratum-circe" % enumeratumVersion )

    val shapeless: Deps = Seq( "com.chuusai" %% "shapeless" % "2.3.9" )

    val java8compat: Deps = Seq( "org.scala-lang.modules" %% "scala-java8-compat" % "1.0.2" )
    val scalaXml: Deps    = Seq( "org.scala-lang.modules" %% "scala-xml" % "2.1.0" )

    val logging: Deps =
      Seq(
        "org.slf4j"      % "slf4j-api"       % "2.0.16",
        "ch.qos.logback" % "logback-classic" % "1.5.8",
        "org.typelevel" %% "log4cats-slf4j"  % "2.7.0"
      )

    val pureconfigVersion = "0.17.7"
    val pureconfig: Deps = "com.github.pureconfig" %% Seq(
      "pureconfig-core",
      "pureconfig-cats",
      "pureconfig-generic"
    ) % pureconfigVersion

    val pureconfigEnumeratum: Deps = Seq( "com.github.pureconfig" %% "pureconfig-enumeratum" % pureconfigVersion )
    val pureconfigCatsEffect: Deps = Seq( "com.github.pureconfig" %% "pureconfig-cats-effect" % pureconfigVersion )
    val pureconfigFs2: Deps        = Seq( "com.github.pureconfig" %% "pureconfig-fs2" % pureconfigVersion )
    val pureconfigHttp4s: Deps     = Seq( "com.github.pureconfig" %% "pureconfig-http4s" % pureconfigVersion )

    private[DependenciesPlugin] val typesafeConfig: Deps = Seq( "com.typesafe" % "config" % "1.4.2" )

    val decline: Deps = "com.monovore" %% Seq( "decline", "decline-effect" ) % "2.4.1"

    val doobieVersion             = "1.0.0-RC6"
    val doobie: Deps              = "org.tpolecat" %% Seq( "doobie-core", "doobie-free" )       % doobieVersion
    val doobiePostgres: Deps      = "org.tpolecat" %% Seq( "doobie-postgres", "doobie-hikari" ) % doobieVersion
    val doobiePostgresCirce: Deps = Seq( "org.tpolecat" %% "doobie-postgres-circe" % doobieVersion )
    val doobieH2: Deps            = Seq( "org.tpolecat" %% "doobie-h2" % doobieVersion )
    val doobieScalatest: Deps     = Seq( "org.tpolecat" %% "doobie-scalatest" % doobieVersion )

    val postgresql: Deps = Seq( "org.postgresql" % "postgresql" % "42.7.4" )
    val flywayCore: Deps = Seq( "org.flywaydb" % "flyway-core" % "9.22.3" )

    val scalatest: Deps = Seq(
      "org.scalatest"     %% "scalatest"       % "3.2.19",
      "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0"
    )

    val scalacheck: Deps =
      Seq(
        "org.scalacheck"    %% "scalacheck"      % "1.18.1",
        "io.chrisdavenport" %% "cats-scalacheck" % "0.3.2"
      )

    val catsLaws: Deps = Seq( "org.typelevel" %% "cats-laws" % catsVersion )
    val discipline: Deps =
      Seq( "org.typelevel" %% "discipline-core" % "1.7.0", "org.typelevel" %% "discipline-scalatest" % "2.3.0" )

    val autoDiffVersion          = "0.6.0"
    val autoDiff: Deps           = "fr.thomasdufour" %% Seq( "auto-diff-core", "auto-diff-generic" ) % autoDiffVersion
    val autoDiffEnumeratum: Deps = Seq( "fr.thomasdufour" %% "auto-diff-enumeratum" % autoDiffVersion )
    val autoDiffScalatest: Deps  = Seq( "fr.thomasdufour" %% "auto-diff-scalatest" % autoDiffVersion )
  }

  import autoImport._

  def allModules: Deps =
    cats ++
      catsFree ++
      catsMtl ++
      catsEffect ++
      mouse ++
      kittens ++
      alleycatsCore ++
      fs2 ++
      http4s ++
      http4sBlazeServer ++
      http4sBlazeClient ++
      http4sPinnedDependencies ++
      monocle ++
//      monocleState ++
//      monocleGeneric ++
      circe ++
      circeFs2 ++
      circeJawn ++
//      circeOptics ++
      circePinnedDependencies ++
      scodec ++
      atto ++
      asciiGraphs ++
      graphs ++
      algebra ++
      spire ++
      algebird ++
      breeze ++
      breezePinnedDependencies ++
      ojAlgo ++
      enumeratum ++
      enumeratumCirce ++
      shapeless ++
      java8compat ++
      scalaXml ++
      logging ++
      pureconfig ++
      pureconfigEnumeratum ++
      pureconfigFs2 ++
      pureconfigHttp4s ++
      typesafeConfig ++
      decline ++
      doobie ++
      doobiePostgres ++
      doobiePostgresCirce ++
      doobieH2 ++
      doobieScalatest ++
      postgresql ++
      flywayCore ++
      scalatest ++
      scalacheck ++
      catsLaws ++
      discipline ++
      autoDiff ++
      autoDiffEnumeratum ++
      autoDiffScalatest

  override def buildSettings: Seq[Def.Setting[_]] =
    ThisBuild / dependencyOverrides ++= Seq(
      "org.scala-lang" % "scala-library"  % scalaVersion.value,
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "org.scala-lang" % "scala-reflect"  % scalaVersion.value
    ) ++ allModules

  override def projectSettings: Seq[Def.Setting[_]] =
    dependencyOverrides ++= Seq(
      "org.scala-lang" % "scala-library"  % scalaVersion.value,
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "org.scala-lang" % "scala-reflect"  % scalaVersion.value
    ) ++ allModules

  class GroupOps( val self: Seq[ModuleID] ) extends AnyVal {
    def exclude( org: String, name: String ): Seq[ModuleID] =
      self.map( _.exclude( org, name ) )

    def %( configurations: String ): Seq[ModuleID] =
      self.map( _ % configurations )

    def classifier( c: String ): Seq[ModuleID] =
      self.map( _ classifier c )
  }

  class StringOps( val self: String ) extends AnyVal {
    def %%( artifactIds: Seq[String] ): Seq[DependencyBuilders.OrganizationArtifactName] = artifactIds.map( self %% _ )
  }

  class DbOanOps( val self: Seq[DependencyBuilders.OrganizationArtifactName] ) extends AnyVal {
    def %( revision: String ): Deps = self.map( _ % revision )
  }

}
