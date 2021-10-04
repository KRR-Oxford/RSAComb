import sbt._

object Dependencies {
  // Versions
  lazy val scalatestVersion = "3.2.3"
  lazy val owlapiVersion = "5.1.17"
  lazy val scalagraphVersion = "1.13.2"
  lazy val ujsonVersion = "1.4.1"
  lazy val oslibVersion = "0.7.8"

  // Libraries
  val scalatest = "org.scalatest" %% "scalatest" % scalatestVersion
  val scalatestFlatSpec =
    "org.scalatest" %% "scalatest-flatspec" % scalatestVersion
  val scalatestShouldMatchers =
    "org.scalatest" %% "scalatest-shouldmatchers" % scalatestVersion
  val apibinding =
    "net.sourceforge.owlapi" % "owlapi-apibinding" % owlapiVersion
  val graphcore = "org.scala-graph" %% "graph-core" % scalagraphVersion
  val ujson = "com.lihaoyi" %% "ujson" % ujsonVersion
  val oslib = "com.lihaoyi" %% "os-lib" % oslibVersion
}
