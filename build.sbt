//import Dependencies._

ThisBuild / scalaVersion     := "2.13.1"
ThisBuild / version          := "0.1.0"
//ThisBuild / organization     := "com.example"
//ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "RSAComb",
    libraryDependencies ++= Seq(
        "org.scalatest" %% "scalatest" % "3.1.0" % "test",
        "net.sourceforge.owlapi" % "owlapi-apibinding" % "5.1.13"
    )
  )
