organization := "com.miriamlaurel"

name := "fxcore"

version := "0.8.5"

scalaVersion := "2.10.3"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test"

scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation", "-feature")