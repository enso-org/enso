// Global Configuration
organization := "org.enso"
scalaVersion := "2.12.8"

// Compiler Options
scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xlint"
)

javacOptions ++= Seq("-source", "12", "-target", "1.8")

// Benchmark Configuration
lazy val Benchmark = config("bench") extend Test
lazy val bench     = taskKey[Unit]("Run Benchmarks")

// Global Project
lazy val enso = (project in file("."))
  .settings(version := "0.1")
  .aggregate(
    syntax,
    pkg,
    interpreter,
    projectManager
  )

// Sub-Projects
lazy val syntax = (project in file("syntax"))
  .settings(
    mainClass in (Compile, run) := Some("org.enso.syntax.Main"),
    version := "0.1"
  )
  .settings(
    libraryDependencies ++= Seq(
      "com.storm-enroute" %% "scalameter" % "0.17" % "bench",
      "org.typelevel"     %% "cats-core"  % "1.6.0",
      "org.scalatest"     %% "scalatest"  % "3.0.5" % Test,
      "com.lihaoyi"       %% "pprint"     % "0.5.3"
    ),
    resolvers ++= Seq(
      "Sonatype OSS Snapshots" at
      "https://oss.sonatype.org/content/repositories/snapshots",
      "Sonatype OSS Releases" at
      "https://oss.sonatype.org/content/repositories/releases"
    )
  )
  .settings(SbtJFlexPlugin.jflexSettings)
  .configs(Test)
  .settings(
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    logBuffered := false
  )
  .configs(Benchmark)
  .settings(
    inConfig(Benchmark)(Defaults.testSettings),
    bench := (test in Benchmark).value,
    parallelExecution in Benchmark := false
  )

lazy val pkg = (project in file("pkg"))
  .settings(
    mainClass in (Compile, run) := Some("org.enso.pkg.Main"),
    version := "0.1"
  )
  .settings(
    libraryDependencies ++= Seq("circe-core", "circe-generic", "circe-yaml")
      .map("io.circe" %% _ % "0.10.0"),
    libraryDependencies += "commons-io" % "commons-io" % "2.6"
  )

val truffleRunOptions = Seq(
  fork := true,
  javaOptions += s"-Dtruffle.class.path.append=${(Compile / classDirectory).value}",
  javaOptions += s"-Dgraal.PrintGraph=Network",
  javaOptions += s"-Dgraal.Dump=Truffle:2",
  javaOptions += s"-Dgraal.TruffleBackgroundCompilation=false",
  javaOptions += s"-Dgraal.TraceTruffleCompilation=true",
  javaOptions += s"-Dgraal.TraceTruffleCompilationCallTree=true",
  javaOptions += s"-Dgraal.TraceTruffleInlining=true",
  javaOptions += s"-Dgraal.TraceTrufflePerformanceWarnings=true",
  javaOptions += s"-Dgraal.TruffleIterativePartialEscape=true",
  javaOptions += s"-XX:-UseJVMCIClassLoader"
)

lazy val interpreter = (project in file("interpreter"))
  .settings(
    mainClass in (Compile, run) := Some("org.enso.interpreter.Main"),
    version := "0.1"
  )
  .settings(
    libraryDependencies ++= Seq(
      "com.chuusai"            %% "shapeless"                % "2.3.3",
      "com.storm-enroute"      %% "scalameter"               % "0.17" % "bench",
      "org.graalvm.sdk"        % "graal-sdk"                 % "19.0.0",
      "org.graalvm.sdk"        % "polyglot-tck"              % "19.0.0",
      "org.graalvm.truffle"    % "truffle-api"               % "19.0.0",
      "org.graalvm.truffle"    % "truffle-dsl-processor"     % "19.0.0",
      "org.graalvm.truffle"    % "truffle-nfi"               % "19.0.0",
      "org.graalvm.truffle"    % "truffle-tck"               % "19.0.0",
      "org.graalvm.truffle"    % "truffle-tck-common"        % "19.0.0",
      "org.scalacheck"         %% "scalacheck"               % "1.14.0" % Test,
      "org.scalatest"          %% "scalatest"                % "3.2.0-SNAP10" % Test,
      "org.scalactic"          %% "scalactic"                % "3.0.8" % Test,
      "com.storm-enroute"      %% "scalameter"               % "0.17" % Benchmark,
      "org.typelevel"          %% "cats-core"                % "2.0.0-M4",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
      "org.apache.commons"     % "commons-lang3"             % "3.9"
    )
  )
  .settings(
    (Compile / javacOptions) ++= Seq(
      "-s",
      (Compile / sourceManaged).value.getAbsolutePath
    )
  )
  .settings(
    (Compile / compile) := (Compile / compile)
      .dependsOn(Def.task { (Compile / sourceManaged).value.mkdirs })
      .value
  )
  .settings(
    inConfig(Compile)(truffleRunOptions),
    inConfig(Test)(truffleRunOptions),
    parallelExecution in Test := false,
    logBuffered in Test := false
  )
  .configs(Benchmark)
  .settings(
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    logBuffered := false,
    inConfig(Benchmark)(Defaults.testSettings),
    inConfig(Benchmark)(truffleRunOptions),
    bench := (test in Benchmark).value,
    parallelExecution in Benchmark := false
  )

val akkaActor  = "com.typesafe.akka" %% "akka-actor"           % "2.5.23"
val akkaStream = "com.typesafe.akka" %% "akka-stream"          % "2.5.23"
val akkaHttp   = "com.typesafe.akka" %% "akka-http"            % "10.1.8"
val akkaSpray  = "com.typesafe.akka" %% "akka-http-spray-json" % "10.1.8"
val akkaTyped  = "com.typesafe.akka" %% "akka-actor-typed"     % "2.5.23"

val akka = Seq(akkaActor, akkaStream, akkaHttp, akkaSpray, akkaTyped)

val circe = Seq("circe-core", "circe-generic", "circe-yaml").map(
  "io.circe" %% _ % "0.10.0"
)

lazy val projectManager = (project in file("project-manager"))
  .settings(
    (Compile / mainClass) := Some("org.enso.projectmanager.Server")
  )
  .settings(
    libraryDependencies ++= akka,
    libraryDependencies ++= circe,
    libraryDependencies += "io.spray" %% "spray-json" % "1.3.5"
  )
  .dependsOn(pkg)
