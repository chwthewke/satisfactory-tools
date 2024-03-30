import sbt.*
import sbt.Def
import sbt.Keys.*

object ScalacPlugin extends AutoPlugin {
  private val warnValueDiscard     = "-Wvalue-discard"
  private val warnNonUnitStatement = "-Wnonunit-statement"
  private val fatalWarnings        = "-Xfatal-warnings"
  private val xLint                = "-Xlint"

  val warningSuppressions: Vector[String] =
    Vector(
      // spurious warning with nested literal partial functions (e.g. nested use of scalatest's inside)
      "cat=other-shadowing&msg=defaultCase\\$:s"
    )

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
    "-Vimplicits",
    s"-Wconf:${warningSuppressions.mkString( "," )}"
  )

  val isIdea: SettingKey[Boolean] = SettingKey( "is-idea", "Whether sbt is run from IntelliJ IDEA" )

  def forTest( opts: Seq[String] ): Seq[String] =
    opts.filterNot( x => x == warnValueDiscard || x == warnNonUnitStatement )

  def forConsole( opts: Seq[String] ): Seq[String] =
    opts.filterNot(
      x => x == fatalWarnings || x == warnValueDiscard || x == warnNonUnitStatement || x.startsWith( xLint )
    )

  override def buildSettings: Seq[Def.Setting[_]] = Seq(
    isIdea := { sys.props.contains( "idea.managed" ) }
  )

  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    // format: off
                        scalacOptions ++= (if (isIdea.value) forTest( options ) else options),
    Compile / console / scalacOptions :=  forConsole( (Compile / console / scalacOptions).value ),
    Test    / compile / scalacOptions :=  forTest( (Compile / compile / scalacOptions).value ),
    Test    / console / scalacOptions :=  forConsole( (Test / compile / scalacOptions).value ),
    Test    / testOptions             +=  Tests.Argument( TestFrameworks.ScalaTest, "-oDF" )
    // format: on
  )

}
