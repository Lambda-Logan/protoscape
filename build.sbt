import Dependencies._

ThisBuild / scalaVersion := "2.13.7"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"

enablePlugins(ScalaJSPlugin)

name := "Scala.js Tutorial"
scalaVersion := "2.13.7" // or any other Scala version >= 2.11.12

// This is an application with a main method
scalaJSUseMainModuleInitializer := true
lazy val root = (project in file("."))
  .settings(
    name := "protoscape",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.1.0",
    libraryDependencies += "org.typelevel" %%% "cats-core" % "2.6.1",
    libraryDependencies += "org.typelevel" %%% "cats-effect" % "2.5.4"
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
