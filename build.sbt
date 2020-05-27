scalaVersion in ThisBuild := "2.13.2"

scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation")

lazy val fxcore = project.in(file(".")).
  settings(
    name := "fxcore",
    organization := "com.miriamlaurel",
    version := "2.8.1-SNAPSHOT",
    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % "3.1.2",
      "org.scalatest" %% "scalatest" % "3.1.2" % "test"
    )
  )


