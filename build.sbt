import Dependencies._

ThisBuild / scalaVersion := "2.13.3"
ThisBuild / organization := "org.dbersenev"
ThisBuild / version := "0.2.0"

lazy val root = (project in file(".")).settings(
  name := "fs2-xml",
  isSnapshot := true,
  libraryDependencies ++= Seq(
    fs2Core, fs2Io, scalaXml
  ),
  libraryDependencies ++= testDeps,
  Compile / discoveredMainClasses := List.empty
)