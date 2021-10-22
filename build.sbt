import Dependencies._

ThisBuild / scalaVersion := "2.13.4"
ThisBuild / version := "0.1.0"
ThisBuild / organization := "uk.ac.ox.cs.rsacomb"
ThisBuild / organizationName := "Department of Computer Science - University of Oxford"
ThisBuild / organizationHomepage := Some(url("https://www.cs.ox.ac.uk"))

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/KRR-Oxford/RSAComb"),
    "scm:git@github.com:KRR-Oxford/RSAComb.git"
  )
)
// ThisBuild / developers := List(
//   Developer(
//     id    = "Your identifier",
//     name  = "Your Name",
//     email = "your@email",
//     url   = url("http://your.url")
//   )
// )

ThisBuild / description := "Re-implementation of the combined approach for CQ answering over RSA ontologies."
ThisBuild / licenses := List(
  "Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt")
)
ThisBuild / homepage := Some(
  url("https://github.com/KRR-Oxford/RSAComb")
)

lazy val root = (project in file("."))
  .settings(
    name := "RSAComb",
    libraryDependencies ++= Seq(
      scalatest % Test,
      scalatestFlatSpec % Test,
      scalatestShouldMatchers % Test,
      apibinding,
      graphcore
    )
  )

/** Exclude RDFox from the building process
  *
  * @see https://github.com/sbt/sbt-assembly#excluding-jars-and-files
  * for different ways to exclude a specific package from the building
  * process.
  */
assemblyExcludedJars in assembly := {
  val cp = (fullClasspath in assembly).value
  cp filter { _.data.getName == "JRDFox.jar" }
}

/** See these links for more info on merging strategies in
  * `sbt-assembly`
  *
  *      https://github.com/sbt/sbt-assembly#merge-strategy
  *      https://stackoverflow.com/a/55557287
  */
assemblyMergeStrategy in assembly := {
  case "module-info.class" =>
    MergeStrategy.discard
  case x => {
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
  }
}
