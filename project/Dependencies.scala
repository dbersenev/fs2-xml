import sbt._

object Dependencies{
  val fs2Core = "co.fs2" %% "fs2-core" % "1.0.4"
  val fs2Io = "co.fs2" %% "fs2-io" % "1.0.4"
  
  val scalaXml = "org.scala-lang.modules" %% "scala-xml" % "1.1.1"

  val scalaTags = "com.lihaoyi" %% "scalatags" % "0.6.7"

  val http4sVersion = "0.19.0"
  val http4s = Seq(
    "org.http4s" %% "http4s-dsl" % http4sVersion,
    "org.http4s" %% "http4s-blaze-server" % http4sVersion,
    "org.http4s" %% "http4s-blaze-client" % http4sVersion
  )
  
  val woodstox = "com.fasterxml.woodstox" % "woodstox-core" % "5.2.0"
}


