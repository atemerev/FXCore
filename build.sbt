lazy val root = (project in file(".")).
  settings(
    name := "fxcore",
    organization := "com.miriamlaurel",
    scalaVersion := "2.11.7",
    version := "2.0",
    sbtVersion := "0.13.9",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.4" % "test"
    )
  )
