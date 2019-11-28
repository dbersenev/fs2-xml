import sbt._

object Dependencies{
  val fs2Core = "co.fs2" %% "fs2-core" % "2.1.0"
  val fs2Io = "co.fs2" %% "fs2-io" % "2.1.0"
  
  val scalaXml = "org.scala-lang.modules" %% "scala-xml" % "1.1.1"

  val scalaTags = "com.lihaoyi" %% "scalatags" % "0.6.7"
  
  val woodstox = "com.fasterxml.woodstox" % "woodstox-core" % "5.2.1"

  val testDeps = List("org.scalatest" %% "scalatest" % "3.0.8" % "test")
}


