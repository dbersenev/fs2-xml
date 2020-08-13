import sbt._

object Dependencies{
  val fs2Core = "co.fs2" %% "fs2-core" % "2.4.2"
  val fs2Io = "co.fs2" %% "fs2-io" % "2.4.2"
  
  val scalaXml = "org.scala-lang.modules" %% "scala-xml" % "1.3.0"
  
  //val woodstox = "com.fasterxml.woodstox" % "woodstox-core" % "6.2.1"

  val testDeps = List("org.scalatest" %% "scalatest" % "3.2.1" % "test")
}


