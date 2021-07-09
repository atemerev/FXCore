lazy val scala2 = "2.13.6"
lazy val supportedScalaVersions = List(scala2)

ThisBuild / organization  :=  "com.miriamlaurel"
ThisBuild / version       :=  "2.8.1-SNAPSHOT"
ThisBuild / scalaVersion  :=  scala2
ThisBuild / scalacOptions ++= Seq("-unchecked", "-deprecation")

lazy val fxcore = project.in(file(".")).
  settings(
    name := "fxcore",
    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % "3.2.9",
      "org.scalatest" %% "scalatest" % "3.2.9" % "test"
    )
  )


