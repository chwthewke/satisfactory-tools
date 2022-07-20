import sbt._
import sbt.Keys._

object ScalacPlugin extends AutoPlugin {
  val options: Seq[String] = Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-feature",
    "-language:higherKinds",
    "-unchecked",
    "-Vimplicits",
    "-Vtype-diffs",
    "-Xfatal-warnings",
    "-Xlint:_,-byname-implicit",
    "-Ywarn-macros:after",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard"
  )

  def forTest( opts: Seq[String] ): Seq[String] =
    opts.filterNot( _ == "-Ywarn-value-discard" )

  def forConsole( opts: Seq[String] ): Seq[String] =
    opts.filterNot( Set( "-Xfatal-warnings", "-Xlint", "-Ywarn-unused-import" ) )

  override def projectSettings: Seq[Def.Setting[_]] =
    // format: off
    Seq(
      scalacOptions                         ++= options,
      Test / scalacOptions                  ~=  forTest,
      Compile / console / scalacOptions     ~=  forConsole,
      Test    / console / scalacOptions     :=  forTest( (Compile / console / scalacOptions).value )
    )
  // format: on
}
