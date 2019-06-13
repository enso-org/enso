version := "1.0"
organization := "org.enso"
scalaVersion := "2.12.8"

lazy val Benchmark = config("bench") extend Test
lazy val bench = taskKey[Unit]("Run Benchmarks")

lazy val enso = (project in file("."))
  .aggregate(syntax, pkg)

lazy val syntax = (project in file("syntax"))
  .configs(Benchmark)
  .settings(
    inConfig(Benchmark)(Defaults.testSettings),
    name := "syntax",
    organization := "org.enso",
    scalaVersion := "2.12.8",
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Xlint"),
    publishArtifact := false,
    libraryDependencies ++= Seq(
      "com.storm-enroute" %% "scalameter" % "0.17" % "bench",
      "org.typelevel" %% "cats-core" % "1.6.0",
      "org.scalatest" %% "scalatest" % "3.0.5" % Test,
      "com.lihaoyi" %% "pprint" % "0.5.3"
    ),
    resolvers ++= Seq(
      "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
      "Sonatype OSS Releases" at "https://oss.sonatype.org/content/repositories/releases"
    ),
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    parallelExecution in Benchmark := false,
    logBuffered := false
  )
  .settings(SbtJFlexPlugin.jflexSettings)
  .settings(mainClass in (Compile, run) := Some("org.enso.syntax.Main"))
  .settings(bench := {
    (test in Benchmark).value
  })

lazy val pkg = (project in file("pkg"))
  .settings(
    mainClass in (Compile, run) := Some("org.enso.pkg.Main"),
    libraryDependencies ++= Seq("circe-core", "circe-generic", "circe-yaml")
      .map("io.circe" %% _ % "0.10.0"),
    libraryDependencies += "commons-io" % "commons-io" % "2.6"
  )
