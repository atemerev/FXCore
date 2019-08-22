import sbt.Keys._

scalaVersion in ThisBuild := "2.13.0"

lazy val fxcore = project.in(file(".")).
  settings(
    name := "fxcore",
    organization := "com.miriamlaurel",
    version := "2.8-SNAPSHOT",
    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % "3.0.8",
      "org.scalatest" %% "scalatest" % "3.0.8" % "test"
    )
  )


