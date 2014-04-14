organization := "com.miriamlaurel"

name := "fxcore"

version := "0.8.6"

scalaVersion := "2.10.4"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test"

scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation", "-feature")