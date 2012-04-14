import sbt._
import Keys._

object BuildSettings {

  val buildOrganization = "com.miriamlaurel"
  val buildVersion      = "0.4.0"
  val buildScalaVersion = "2.9.1"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := buildOrganization,
    version      := buildVersion,
    scalaVersion := buildScalaVersion,
    shellPrompt  := ShellPrompt.buildShellPrompt,
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "1.7.1" % "test"
    ),
    resolvers ++= Seq(
      "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases"
    )
  )
}

object ShellPrompt {
  object devnull extends ProcessLogger {
    def info (s: => String) {}
    def error (s: => String) { }
    def buffer[T] (f: => T): T = f
  }
  def currBranch = (
    ("git status -sb" lines_! devnull headOption)
      getOrElse "-" stripPrefix "## "
  )

  val buildShellPrompt = {
    (state: State) => {
      val currProject = Project.extract (state).currentProject.id
      "%s %s %s> ".format (
        currProject, currBranch, BuildSettings.buildVersion
      )
    }
  }
}

object FxcoreBuild extends Build {

  import BuildSettings._

  lazy val fxcore = Project(
    id = "fxcore",
    base = file("."),
    settings = buildSettings
  )
}