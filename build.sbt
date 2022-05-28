lazy val scala2 = "2.13.8"
lazy val supportedScalaVersions = List(scala2)

ThisBuild / organization  :=  "ai.reactivity"
ThisBuild / version       :=  "2.9.0-SNAPSHOT"
ThisBuild / scalaVersion  :=  scala2
ThisBuild / scalacOptions ++= Seq("-unchecked", "-deprecation")

lazy val fxcore = project.in(file(".")).
  settings(
    name := "fxcore",
    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % "3.2.12",
      "org.scalatest" %% "scalatest" % "3.2.12" % "test"
    )
  )


