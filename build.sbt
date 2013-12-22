organization := "com.miriamlaurel"

name := "fxcore"

version := "0.8"

scalaVersion := "2.10.3"

resolvers ++= Seq(
  "ML Repository" at "http://miriamlaurel.com:8080/archiva/repository/internal/"
)

libraryDependencies ++= Seq(
    "org.scalatest" % "scalatest_2.10" % "2.0" % "test"
)

scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation", "-feature")