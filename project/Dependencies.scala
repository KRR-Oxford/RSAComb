import sbt._

object Dependencies {
  // Versions
  lazy val scalatestVersion = "3.2.2"
  lazy val owlapiVersion = "5.1.16"
  lazy val scalagraphVersion = "1.13.2"

  // Libraries
  val scalatest = "org.scalatest" %% "scalatest" % scalatestVersion
  val scalatestFlatSpec = "org.scalatest" %% "scalatest-flatspec" % scalatestVersion
  val scalatestShouldMatchers = "org.scalatest" %% "scalatest-shouldmatchers" % scalatestVersion
  val apibinding = "net.sourceforge.owlapi" % "owlapi-apibinding" % owlapiVersion
  val graphcore = "org.scala-graph" %% "graph-core" % scalagraphVersion
}
