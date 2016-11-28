import sbt.Keys._

scalaVersion in ThisBuild := "2.11.8"
crossScalaVersions := Seq("2.11.8", "2.12.0")
sbtVersion in ThisBuild := "0.13.13"

lazy val root = project.in(file(".")).
  aggregate(js, jvm).settings(
  name := "fxcore",
  organization := "com.miriamlaurel",
  version := "2.4-SNAPSHOT"
)

lazy val fxcore = crossProject.in(file(".")).
  settings(
    name := "fxcore",
    organization := "com.miriamlaurel",
    version := "2.4-SNAPSHOT"
  ).jvmSettings(
    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % "3.0.1",
      "org.scalatest" %% "scalatest" % "3.0.1" % "test"
    )
  ).jsSettings(
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest" % "3.0.1" % "test"
    )
  )

lazy val jvm = fxcore.jvm
lazy val js = fxcore.js

