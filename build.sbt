import sbt.Keys._

scalaVersion in ThisBuild := "2.12.5"

lazy val root = project.in(file(".")).
  aggregate(js, jvm).settings(
  name := "fxcore",
  organization := "com.miriamlaurel",
  version := "2.6-SNAPSHOT"
)

lazy val fxcore = crossProject.in(file(".")).
  settings(
    name := "fxcore",
    organization := "com.miriamlaurel",
    version := "2.6-SNAPSHOT"
  ).jvmSettings(
    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % "3.0.3",
      "org.scalatest" %% "scalatest" % "3.0.1" % "test"
    )
  ).jsSettings(
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest" % "3.0.3" % "test"
    )
  )

lazy val jvm = fxcore.jvm
lazy val js = fxcore.js

