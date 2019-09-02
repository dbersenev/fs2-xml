import Dependencies._

val commons = Seq(
  scalaVersion := "2.12.8"
)

lazy val root = (project in file(".")).settings(
  commons,
  name := "fs2-xml",
  organization := "org.dbersenev",
  version := "0.1.5",
  libraryDependencies ++= Seq(
    fs2Core, fs2Io, scalaXml, woodstox
  ),
  scalacOptions += "-Ypartial-unification",
  mainClass in Compile := Some("org.dbersenev.fs2.xml.OrcidTest"),
  executableScriptName := "run",
  Compile / discoveredMainClasses := List.empty
).enablePlugins(JavaAppPackaging)