import scala.sys.process._

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
lazy val benchOnly = inputKey[Unit]("Run benchmarks by name substring")
lazy val buildNativeImage =
  taskKey[Unit]("Build native image for the Enso executable")

// Global Project
lazy val enso = (project in file("."))
  .settings(version := "0.1")
  .aggregate(
    syntax,
    pkg,
    interpreter,
    projectManager,
    fileManager
  )

// Sub-Projects
lazy val syntax = (project in file("Syntax"))
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

lazy val pkg = (project in file("Pkg"))
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
  javaOptions += s"-Dgraal.TruffleIterativePartialEscape=true",
  javaOptions += s"-XX:-UseJVMCIClassLoader",
  javaOptions += s"-Dgraal.TruffleBackgroundCompilation=false"
)

val jmh = Seq(
  "org.openjdk.jmh" % "jmh-core"                 % "1.21" % Benchmark,
  "org.openjdk.jmh" % "jmh-generator-annprocess" % "1.21" % Benchmark
)

lazy val interpreter = (project in file("Interpreter"))
  .settings(
    mainClass in (Compile, run) := Some("org.enso.interpreter.Main"),
    version := "0.1"
  )
  .settings(commands += RunDebugCommand.runDebug)
  .settings(
    libraryDependencies ++= Seq(
      "com.chuusai"            %% "shapeless"                % "2.3.3",
      "org.apache.commons"     % "commons-lang3"             % "3.9",
      "org.apache.tika"        % "tika-core"                 % "1.21",
      "org.graalvm.sdk"        % "graal-sdk"                 % "19.2.0",
      "org.graalvm.sdk"        % "polyglot-tck"              % "19.2.0",
      "org.graalvm.truffle"    % "truffle-api"               % "19.2.0",
      "org.graalvm.truffle"    % "truffle-dsl-processor"     % "19.2.0",
      "org.graalvm.truffle"    % "truffle-tck"               % "19.2.0",
      "org.graalvm.truffle"    % "truffle-tck-common"        % "19.2.0",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
      "org.scalacheck"         %% "scalacheck"               % "1.14.0" % Test,
      "org.scalactic"          %% "scalactic"                % "3.0.8" % Test,
      "org.scalatest"          %% "scalatest"                % "3.2.0-SNAP10" % Test,
      "org.typelevel"          %% "cats-core"                % "2.0.0-M4",
      "commons-cli"            % "commons-cli"               % "1.4"
    ),
    libraryDependencies ++= jmh
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
  .settings(
    buildNativeImage := Def
      .task {
        val javaHome        = System.getProperty("java.home")
        val nativeImagePath = s"$javaHome/bin/native-image"
        val classPath       = (Runtime / fullClasspath).value.files.mkString(":")
        val cmd =
          s"$nativeImagePath --macro:truffle --no-fallback --initialize-at-build-time -cp $classPath ${(Compile / mainClass).value.get} enso"
        cmd !
      }
      .dependsOn(Compile / compile)
      .value
  )
  .configs(Benchmark)
  .settings(
    logBuffered := false,
    inConfig(Benchmark)(Defaults.testSettings),
    inConfig(Benchmark)(truffleRunOptions),
    bench := (test in Benchmark).value,
    benchOnly := Def.inputTaskDyn {
      import complete.Parsers.spaceDelimited
      val name = spaceDelimited("<name>").parsed match {
        case List(name) => name
        case _          => throw new IllegalArgumentException("Expected one argument.")
      }
      Def.task {
        (testOnly in Benchmark).toTask(" -- -z " + name).value
      }
    }.evaluated,
    parallelExecution in Benchmark := false
  )
  .dependsOn(pkg)

val akkaActor        = "com.typesafe.akka" %% "akka-actor"               % "2.5.23"
val akkaStream       = "com.typesafe.akka" %% "akka-stream"              % "2.5.23"
val akkaHttp         = "com.typesafe.akka" %% "akka-http"                % "10.1.8"
val akkaSpray        = "com.typesafe.akka" %% "akka-http-spray-json"     % "10.1.8"
val akkaTyped        = "com.typesafe.akka" %% "akka-actor-typed"         % "2.5.23"
val akkaTestkit      = "com.typesafe.akka" %% "akka-testkit"             % "2.5.23"
val akkaSLF4J        = "com.typesafe.akka" %% "akka-slf4j"               % "2.5.23"
val akkaTestkitTyped = "com.typesafe.akka" %% "akka-actor-testkit-typed" % "2.5.23" % Test

val akka = Seq(akkaActor, akkaStream, akkaHttp, akkaSpray, akkaTyped)

val circe = Seq("circe-core", "circe-generic", "circe-yaml").map(
  "io.circe" %% _ % "0.10.0"
)

lazy val fileManager = (project in file("FileManager"))
  .settings(
    (Compile / mainClass) := Some("org.enso.filemanager.FileManager")
  )
  .settings(
    libraryDependencies ++= akka,
    libraryDependencies += akkaSLF4J,
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3",
    libraryDependencies += "org.scalatest"  %% "scalatest"      % "3.2.0-SNAP10" % Test,
    libraryDependencies += "org.scalacheck" %% "scalacheck"     % "1.14.0" % Test,
    libraryDependencies += akkaTestkitTyped,
    libraryDependencies += "commons-io" % "commons-io"        % "2.6",
    libraryDependencies += "io.methvin" % "directory-watcher" % "0.9.6"
  )

lazy val projectManager = (project in file("ProjectManager"))
  .settings(
    (Compile / mainClass) := Some("org.enso.projectmanager.Server")
  )
  .settings(
    libraryDependencies ++= akka,
    libraryDependencies ++= circe,
    libraryDependencies += "io.spray" %% "spray-json" % "1.3.5"
  )
  .dependsOn(pkg)
