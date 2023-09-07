// See README.md for license details.

ThisBuild / scalaVersion := "2.13.8"
ThisBuild / version := "0.1.0"
ThisBuild / organization := "%ORGANIZATION%"

val chiselVersion = "5.0.0"

lazy val root = (project in file("."))
  .settings(
    name := "chisel-fish",
    libraryDependencies ++= Seq(
      "org.chipsalliance" %% "chisel" % chiselVersion,
      "edu.berkeley.cs" %% "chiseltest" % "5.0.0" % "test",
      "net.fornwall" % "jelf" % "0.7.0",
      "org.scalacheck" %% "scalacheck" % "1.17.0" % "test"
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
      "-Ymacro-annotations",
      "-P:chiselplugin:genBundleElements"
    ),
    addCompilerPlugin(
      "org.chipsalliance" % "chisel-plugin" % chiselVersion cross CrossVersion.full
    )
  )
