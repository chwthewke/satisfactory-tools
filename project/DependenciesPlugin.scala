import sbt._
import sbt.Keys._
import sbt.librarymanagement.DependencyBuilders
import scala.language.implicitConversions

object DependenciesPlugin extends AutoPlugin {
  type Deps = Seq[ModuleID]

  private implicit def toDeps( dependency: ModuleID ): Seq[ModuleID] = Seq( dependency )

  object autoImport {
    type Deps = DependenciesPlugin.Deps

    implicit def ToStringOps( orgName: String ): StringOps = new StringOps( orgName )

    implicit def ToDbOanOps( dbOans: Seq[DbOan] ): DbOanOps = new DbOanOps( dbOans )

    implicit def ToGroupOps( deps: Deps ): GroupOps = new GroupOps( deps )

    val kindProjector: Deps =
      compilerPlugin( "org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full )

    val betterMonadicFor: Deps = compilerPlugin( "com.olegpy" %% "better-monadic-for" % "0.3.1" )

    val catsVersion         = "2.7.0"
    val cats: Deps          = "org.typelevel" %% Seq( "cats-core", "cats-kernel" ) % catsVersion
    val catsFree: Deps      = "org.typelevel" %% "cats-free" % catsVersion
    val catsMtl: Deps       = "org.typelevel" %% "cats-mtl-core" % "0.7.1"
    val mouse: Deps         = "org.typelevel" %% "mouse" % "1.0.9"
    val kittens: Deps       = "org.typelevel" %% "kittens" % "2.3.2"
    val alleycatsCore: Deps = "org.typelevel" %% "alleycats-core" % catsVersion
    val catsTime: Deps      = "io.chrisdavenport" %% "cats-time" % "0.4.0"
    val catsParse: Deps     = "org.typelevel" %% "cats-parse" % "0.3.6"

    val catsEffectVersion      = "3.3.5"
    val catsEffectKernel: Deps = "org.typelevel" %% "cats-effect-kernel" % catsEffectVersion
    val catsEffect
        : Deps = "org.typelevel" %% Seq( "cats-effect", "cats-effect-kernel", "cats-effect-std" ) % catsEffectVersion

    private val fs2Version = "3.2.4"
    val fs2: Deps          = "co.fs2" %% Seq( "fs2-core", "fs2-io" ) % fs2Version
    val fs2Scodec: Deps    = "co.fs2" %% "fs2-scodec" % fs2Version

    val http4sVersion           = "0.23.9"
    val http4s: Deps            = "org.http4s" %% "http4s-dsl" % http4sVersion
    val http4sBlazeServer: Deps = "org.http4s" %% "http4s-blaze-server" % http4sVersion
    val http4sBlazeClient: Deps = "org.http4s" %% "http4s-blaze-client" % http4sVersion

    val http4sPinnedDependencies: Deps = Seq(
//      "com.comcast"   %% "ip4s-core" % "3.0.2",
//      "org.typelevel" %% "literally" % "1.0.1"
    )

    val scalatags: Deps = Seq(
      "com.lihaoyi" %% "scalatags"        % "0.11.1",
      "org.http4s"  %% "http4s-scalatags" % http4sVersion
    )

    val monocleVersion       = "2.0.4"
    val monocle: Deps        = "com.github.julien-truffaut" %% Seq( "monocle-core", "monocle-macro" ) % monocleVersion
    val monocleState: Deps   = "com.github.julien-truffaut" %% "monocle-state" % monocleVersion
    val monocleGeneric: Deps = "com.github.julien-truffaut" %% "monocle-generic" % monocleVersion

    val circeVersion      = "0.14.1"
    val circe: Deps       = "io.circe" %% Seq( "circe-core", "circe-generic", "circe-parser" ) % circeVersion
    val circeOptics: Deps = "io.circe" %% "circe-optics" % circeVersion
    val circeFs2: Deps    = "io.circe" %% "circe-fs2" % "0.14.0"
    val circeJawn: Deps   = "io.circe" %% "circe-jawn" % circeVersion
    val jawnParser: Deps  = "org.typelevel" %% "jawn-parser" % "1.1.2"

    val scodec: Deps = Seq(
      "org.scodec" %% "scodec-bits" % "1.1.30",
      "org.scodec" %% "scodec-cats" % "1.1.0",
      "org.scodec" %% "scodec-core" % "1.11.9"
    )

    val atto: Deps = "org.tpolecat" %% "atto-core" % "0.9.5"

    val asciiGraphs: Deps = "org.scalameta" %% "ascii-graphs"                      % "0.1.2"
    val graphs: Deps      = "com.flowtick"  %% Seq( "graphs-core", "graphs-cats" ) % "0.5.0"

    val spire: Deps    = "org.typelevel" %% "spire"         % "0.17.0"
    val algebird: Deps = "com.twitter"   %% "algebird-core" % "0.13.6"
    val algebra: Deps  = "org.typelevel" %% "algebra"       % "2.0.0"

    val breeze: Deps = "org.scalanlp" %% Seq( "breeze", "breeze-natives" ) % "1.1"
    val breezePinnedDependencies: Deps =
      Seq(
        "org.apache.commons"       % "commons-math3" % "3.5",
        "com.github.fommil.netlib" % "core"          % "1.1.2"
      )

    val ojAlgo: Deps = "org.ojalgo" % "ojalgo" % "50.0.2"

    val enumeratumVersion: String = "1.7.0"
    val enumeratum: Deps =
      "com.beachape" %% Seq( "enumeratum", "enumeratum-cats" ) % enumeratumVersion
    val enumeratumCirce: Deps = "com.beachape" %% "enumeratum-circe" % enumeratumVersion

    val shapeless: Deps = "com.chuusai" %% "shapeless" % "2.3.7"

    val java8compat: Deps = "org.scala-lang.modules" %% "scala-java8-compat" % "1.0.0"
    val scalaXml: Deps    = "org.scala-lang.modules" %% "scala-xml"          % "2.0.1"

    val logging: Deps =
      Seq(
        "org.slf4j"      % "slf4j-api"       % "1.7.35",
        "ch.qos.logback" % "logback-classic" % "1.2.10",
        "org.typelevel"  %% "log4cats-slf4j" % "2.2.0"
      )

    val pureconfigVersion = "0.17.1"
    val pureconfig: Deps = "com.github.pureconfig" %% Seq(
      "pureconfig-core",
      "pureconfig-cats",
      "pureconfig-generic"
    ) % pureconfigVersion

    val pureconfigEnumeratum: Deps = "com.github.pureconfig" %% "pureconfig-enumeratum"  % pureconfigVersion
    val pureconfigCatsEffect: Deps = "com.github.pureconfig" %% "pureconfig-cats-effect" % pureconfigVersion
    val pureconfigFs2: Deps        = "com.github.pureconfig" %% "pureconfig-fs2"         % pureconfigVersion
    val pureconfigHttp4s: Deps     = "com.github.pureconfig" %% "pureconfig-http4s"      % pureconfigVersion

    private[DependenciesPlugin] val typesafeConfig: Deps = "com.typesafe" % "config" % "1.4.1"

    val decline: Deps = "com.monovore" %% Seq( "decline", "decline-effect" ) % "2.2.0"

    val doobieVersion             = "1.0.0-RC2"
    val doobie: Deps              = "org.tpolecat" %% Seq( "doobie-core", "doobie-free" ) % doobieVersion
    val doobiePostgres: Deps      = "org.tpolecat" %% Seq( "doobie-postgres", "doobie-hikari" ) % doobieVersion
    val doobiePostgresCirce: Deps = "org.tpolecat" %% "doobie-postgres-circe" % doobieVersion
    val doobieH2: Deps            = "org.tpolecat" %% "doobie-h2" % doobieVersion
    val doobieScalatest: Deps     = "org.tpolecat" %% "doobie-scalatest" % doobieVersion

    val postgresql: Deps = "org.postgresql" % "postgresql"  % "42.3.1"
    val flywayCore: Deps = "org.flywaydb"   % "flyway-core" % "8.4.3"

    val scalatest: Deps = Seq(
      "org.scalatest"     %% "scalatest"       % "3.2.11",
      "org.scalatestplus" %% "scalacheck-1-15" % "3.2.11.0"
    )

    val scalacheck: Deps =
      Seq(
        "org.scalacheck"    %% "scalacheck"      % "1.15.4",
        "io.chrisdavenport" %% "cats-scalacheck" % "0.3.1"
      )

    val catsLaws: Deps = "org.typelevel" %% "cats-laws" % catsVersion
    val discipline: Deps =
      Seq( "org.typelevel" %% "discipline-core" % "1.4.0", "org.typelevel" %% "discipline-scalatest" % "2.1.5" )

    val autoDiffVersion          = "0.5.1"
    val autoDiff: Deps           = "fr.thomasdufour" %% Seq( "auto-diff-core", "auto-diff-generic" ) % autoDiffVersion
    val autoDiffEnumeratum: Deps = "fr.thomasdufour" %% "auto-diff-enumeratum" % autoDiffVersion
    val autoDiffScalatest: Deps  = "fr.thomasdufour" %% "auto-diff-scalatest" % autoDiffVersion
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
      scalatags ++
      monocle ++
      monocleState ++
      monocleGeneric ++
      circe ++
      circeFs2 ++
      circeJawn ++
      circeOptics ++
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
      jawnParser ++
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

  type DbOan = DependencyBuilders.OrganizationArtifactName

  class GroupOps( val self: Seq[ModuleID] ) extends AnyVal {
    def exclude( org: String, name: String ): Seq[ModuleID] =
      self.map( _.exclude( org, name ) )

    def %( configurations: String ): Seq[ModuleID] =
      self.map( _ % configurations )

    def classifier( c: String ): Seq[ModuleID] =
      self.map( _ classifier c )
  }

  class StringOps( val self: String ) extends AnyVal {
    def %%( artifactIds: Seq[String] ): Seq[DbOan] = artifactIds.map( self %% _ )
  }

  class DbOanOps( val self: Seq[DbOan] ) extends AnyVal {
    def %( revision: String ): Deps = self.map( _ % revision )
  }

}
