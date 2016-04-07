lazy val root = (project in file(".")).
  settings(
    name := "fxcore",
    organization := "com.miriamlaurel",
    scalaVersion := "2.11.8",
    version := "2.0",
    sbtVersion := "0.13.11",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.6" % "test"
    )
  )
