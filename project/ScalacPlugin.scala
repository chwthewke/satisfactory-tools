import sbt._
import sbt.Keys._

object ScalacPlugin extends AutoPlugin {
  private val warnValueDiscard     = "-Wvalue-discard"
  private val warnNonUnitStatement = "-Wnonunit-statement"
  private val fatalWarnings        = "-Xfatal-warnings"
  private val xLint                = "-Xlint"

  val options: Seq[String] = Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-feature",
    "-language:higherKinds",
    "-unchecked",
    warnValueDiscard,
    warnNonUnitStatement,
    fatalWarnings,
    s"$xLint:-byname-implicit,_",
    "-Ywarn-macros:after",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Vimplicits"
  )

  val isIdea: SettingKey[Boolean] = SettingKey( "is-idea", "Whether sbt is run from IntelliJ IDEA" )

  def forTest( opts: Seq[String] ): Seq[String] =
    opts.filterNot( x => x == warnValueDiscard || x == warnNonUnitStatement )

  def forConsole( opts: Seq[String] ): Seq[String] =
    opts.filterNot(
      x => x == fatalWarnings || x == warnValueDiscard || x == warnNonUnitStatement || x.startsWith( xLint )
    )

  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    // format: off
                        scalacOptions ++= options,
    Compile / console / scalacOptions :=  forConsole( (Compile / console / scalacOptions).value ),
    Test    / compile / scalacOptions :=  forTest( (Compile / compile / scalacOptions).value ),
    Test    / console / scalacOptions :=  forConsole( (Test / compile / scalacOptions).value ),
    Test    / testOptions             +=  Tests.Argument( TestFrameworks.ScalaTest, "-oDF" )
    // format: on
  )

}
