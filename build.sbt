import sbt.Keys.parallelExecution
import sbt.Test
// See README.md for license details.

ThisBuild / scalaVersion := "2.13.12"
ThisBuild / version := "0.1.0"
ThisBuild / organization := "%ORGANIZATION%"

val chiselVersion = "6.3.0"
//val chiselVersion = "6.0.0-RC1"

lazy val root = (project in file("."))
  .settings(
    name := "chisel-fish",
    Test / parallelExecution := false,
    libraryDependencies ++= Seq(
      "org.chipsalliance" %% "chisel" % chiselVersion,
      "edu.berkeley.cs" %% "chiseltest" % "6.0.0",
      "net.fornwall" % "jelf" % "0.7.0",
      "org.typelevel" %% "spire" % "0.18.0",
      "org.scalacheck" %% "scalacheck" % "1.17.0"
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
      "-Ymacro-annotations"
    ),
    addCompilerPlugin(
      "org.chipsalliance" % "chisel-plugin" % chiselVersion cross CrossVersion.full
    )
  )
