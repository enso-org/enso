import java.io.File

import org.enso.build.BenchTasks._
import org.enso.build.WithDebugCommand
import sbt.Keys.scalacOptions
import sbt.addCompilerPlugin
import sbtassembly.AssemblyPlugin.defaultUniversalScript
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

import scala.sys.process._

//////////////////////////////
//// Global Configuration ////
//////////////////////////////

val scalacVersion = "2.13.1"
val graalVersion  = "20.0.0"
val circeVersion  = "0.13.0"
val ensoVersion   = "0.0.1"
organization in ThisBuild := "org.enso"
scalaVersion in ThisBuild := scalacVersion

Global / onChangedBuildSource := ReloadOnSourceChanges

//////////////////////////
//// Compiler Options ////
//////////////////////////

javacOptions in ThisBuild ++= Seq(
  "-encoding",   // Provide explicit encoding (the next line)
  "UTF-8",       // Specify character encoding used by Java source files.
  "-deprecation" // Shows a description of each use or override of a deprecated member or class.
)

scalacOptions in ThisBuild ++= Seq(
  "-deprecation",                       // Emit warning and location for usages of deprecated APIs.
  "-encoding",                          // Provide explicit encoding (the next line)
  "utf-8",                              // Specify character encoding used by Scala source files.
  "-explaintypes",                      // Explain type errors in more detail.
  "-feature",                           // Emit warning and location for usages of features that should be imported explicitly.
  "-language:existentials",             // Existential types (besides wildcard types) can be written and inferred
  "-language:experimental.macros",      // Allow macro definition (besides implementation and application)
  "-language:higherKinds",              // Allow higher-kinded types
  "-language:implicitConversions",      // Allow definition of implicit functions called views
  "-unchecked",                         // Enable additional warnings where generated code depends on assumptions.
  "-Xcheckinit",                        // Wrap field accessors to throw an exception on uninitialized access.
  "-Xlint:adapted-args",                // Warn if an argument list is modified to match the receiver.
  "-Xlint:constant",                    // Evaluation of a constant arithmetic expression results in an error.
  "-Xlint:delayedinit-select",          // Selecting member of DelayedInit.
  "-Xlint:doc-detached",                // A Scaladoc comment appears to be detached from its element.
  "-Xlint:inaccessible",                // Warn about inaccessible types in method signatures.
  "-Xlint:infer-any",                   // Warn when a type argument is inferred to be `Any`.
  "-Xlint:missing-interpolator",        // A string literal appears to be missing an interpolator id.
  "-Xlint:nullary-override",            // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Xlint:nullary-unit",                // Warn when nullary methods return Unit.
  "-Xlint:option-implicit",             // Option.apply used implicit view.
  "-Xlint:package-object-classes",      // Class or object defined in package object.
  "-Xlint:poly-implicit-overload",      // Parameterized overloaded implicit methods are not visible as view bounds.
  "-Xlint:private-shadow",              // A private field (or class parameter) shadows a superclass field.
  "-Xlint:stars-align",                 // Pattern sequence wildcard must align with sequence component.
  "-Xlint:type-parameter-shadow",       // A local type parameter shadows a type already in scope.
  "-Xmacro-settings:-logging@org.enso", // Disable the debug logging globally.
  "-Ywarn-dead-code",                   // Warn when dead code is identified.
  "-Ywarn-extra-implicit",              // Warn when more than one implicit parameter section is defined.
  "-Ywarn-numeric-widen",               // Warn when numerics are widened.
  "-Ywarn-unused:imports",              // Warn if an import selector is not referenced.
  "-Ywarn-unused:implicits",            // Warn if an implicit parameter is unused.
  "-Ywarn-unused:locals",               // Warn if a local definition is unused.
  "-Ywarn-unused:patvars",              // Warn if a variable bound in a pattern is unused.
  "-Ywarn-unused:privates",             // Warn if a private member is unused.
  "-Ywarn-unused:params",               // Warn if a value parameter is unused.
  "-Ywarn-value-discard"                // Warn when non-Unit expression results are unused.
)

/////////////////////////////////
//// Benchmark Configuration ////
/////////////////////////////////

lazy val Benchmark = config("bench") extend sbt.Test

// Native Image Generation
lazy val buildNativeImage =
  taskKey[Unit]("Build native image for the Enso executable")

////////////////////////
//// Global Project ////
////////////////////////

lazy val enso = (project in file("."))
  .settings(version := "0.1")
  .aggregate(
    unused.jvm,
    flexer.jvm,
    syntax_definition.jvm,
    syntax.jvm,
    pkg,
    runtime,
    polyglot_api,
    parser_service,
    file_manager,
    project_manager,
    graph,
    runner,
    language_server
  )
  .settings(Global / concurrentRestrictions += Tags.exclusive(Exclusive))

////////////////////////////
//// Dependency Bundles ////
////////////////////////////

val monocle = {
  val monocleVersion = "2.0.0"
  Seq(
    "com.github.julien-truffaut" %% "monocle-core"  % monocleVersion,
    "com.github.julien-truffaut" %% "monocle-macro" % monocleVersion,
    "com.github.julien-truffaut" %% "monocle-law"   % monocleVersion % "test"
  )
}

val catsVersion = "2.1.0"
val cats = {
  Seq(
    "org.typelevel" %% "cats-core"   % catsVersion,
    "org.typelevel" %% "cats-effect" % catsVersion,
    "org.typelevel" %% "cats-free"   % catsVersion,
    "org.typelevel" %% "cats-macros" % catsVersion,
    "org.typelevel" %% "kittens"     % catsVersion
  )
}

val scala_compiler = Seq(
  "org.scala-lang" % "scala-reflect"  % scalacVersion,
  "org.scala-lang" % "scala-compiler" % scalacVersion
)

val circe = Seq("circe-core", "circe-generic", "circe-parser")
  .map("io.circe" %% _ % circeVersion)

def akkaPkg(name: String)     = akkaURL %% s"akka-$name" % akkaVersion
def akkaHTTPPkg(name: String) = akkaURL %% s"akka-$name" % akkaHTTPVersion

val akkaURL          = "com.typesafe.akka"
val akkaVersion      = "2.6.3"
val akkaHTTPVersion  = "10.1.11"
val akkaActor        = akkaPkg("actor")
val akkaStream       = akkaPkg("stream")
val akkaTyped        = akkaPkg("actor-typed")
val akkaTestkit      = akkaPkg("testkit")
val akkaSLF4J        = akkaPkg("slf4j")
val akkaTestkitTyped = akkaPkg("actor-testkit-typed") % Test
val akkaHttp         = akkaHTTPPkg("http")
val akkaSpray        = akkaHTTPPkg("http-spray-json")
val akka             = Seq(akkaActor, akkaStream, akkaHttp, akkaSpray, akkaTyped)

val jmh = Seq(
  "org.openjdk.jmh" % "jmh-core"                 % "1.21" % Benchmark,
  "org.openjdk.jmh" % "jmh-generator-annprocess" % "1.21" % Benchmark
)

val silencerVersion = "1.4.4"

////////////////////////////
//// Internal Libraries ////
////////////////////////////

val jsSettings = Seq(
  scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
  // FIXME workaround for scalajs bug:
  //  https://github.com/scala-js/scala-js/issues/3673
  testFrameworks := Nil
)

lazy val logger = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("common/logger"))
  .dependsOn(unused)
  .settings(
    version := "0.1",
    libraryDependencies ++= scala_compiler
  )
  .jsSettings(jsSettings)

lazy val flexer = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("common/flexer"))
  .dependsOn(logger)
  .settings(
    version := "0.1",
    resolvers += Resolver.sonatypeRepo("releases"),
    libraryDependencies ++= scala_compiler ++ Seq(
      "com.google.guava" % "guava"       % "28.2-jre",
      "org.typelevel"    %%% "cats-core" % catsVersion,
      "org.typelevel"    %%% "kittens"   % "2.0.0"
    )
  )
  .jsSettings(jsSettings)

lazy val unused = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("common/unused"))
  .settings(version := "0.1", scalacOptions += "-nowarn")
  .jsSettings(testFrameworks := Nil)

lazy val syntax_definition = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("common/syntax/definition"))
  .dependsOn(logger, flexer)
  .settings(
    libraryDependencies ++= monocle ++ scala_compiler ++ Seq(
      "org.typelevel" %%% "cats-core"     % catsVersion,
      "org.typelevel" %%% "kittens"       % "2.0.0",
      "com.lihaoyi"   %%% "scalatags"     % "0.8.5",
      "io.circe"      %%% "circe-core"    % circeVersion,
      "io.circe"      %%% "circe-generic" % circeVersion,
      "io.circe"      %%% "circe-parser"  % circeVersion
    )
  )
  .jsSettings(jsSettings)

lazy val syntax = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("common/syntax/specialization"))
  .dependsOn(logger, flexer, syntax_definition)
  .configs(Test)
  .configs(Benchmark)
  .settings(
    testFrameworks := Nil,
    mainClass in (Compile, run) := Some("org.enso.syntax.text.Main"),
    version := "0.1",
    logBuffered := false,
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest"     % "3.1.0" % Test,
      "com.lihaoyi"   %%% "pprint"        % "0.5.9",
      "io.circe"      %%% "circe-core"    % circeVersion,
      "io.circe"      %%% "circe-generic" % circeVersion,
      "io.circe"      %%% "circe-parser"  % circeVersion
    ),
    compile := (Compile / compile)
      .dependsOn(Def.taskDyn {
        val parserCompile =
          (syntax_definition.jvm / Compile / compileIncremental).value
        if (parserCompile.hasModified) {
          Def.task {
            streams.value.log.info("Parser changed, forcing recompilation.")
            clean.value
          }
        } else Def.task {}
      })
      .value
  )
  .jvmSettings(
    inConfig(Benchmark)(Defaults.testSettings),
    unmanagedSourceDirectories in Benchmark +=
    baseDirectory.value.getParentFile / "shared/src/bench/scala",
    libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.19" % "bench",
    testFrameworks := List(
      new TestFramework("org.scalatest.tools.Framework"),
      new TestFramework("org.scalameter.ScalaMeterFramework")
    ),
    bench := (test in Benchmark).tag(Exclusive).value
  )
  .jsSettings(
    scalaJSUseMainModuleInitializer := false,
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    testFrameworks := List(new TestFramework("org.scalatest.tools.Framework")),
    Compile / fullOptJS / artifactPath := file("target/scala-parser.js")
  )

lazy val parser_service = (project in file("common/parser-service"))
  .dependsOn(syntax.jvm)
  .settings(
    libraryDependencies ++= akka,
    mainClass := Some("org.enso.ParserServiceMain")
  )

lazy val graph = (project in file("common/graph/"))
  .dependsOn(logger.jvm)
  .configs(Test)
  .settings(
    version := "0.1",
    resolvers ++= Seq(
      Resolver.sonatypeRepo("releases"),
      Resolver.sonatypeRepo("snapshots")
    ),
    scalacOptions += "-Ymacro-annotations",
    libraryDependencies ++= scala_compiler ++ Seq(
      "com.chuusai"                %% "shapeless"    % "2.3.3",
      "io.estatico"                %% "newtype"      % "0.4.3",
      "org.scalatest"              %% "scalatest"    % "3.2.0-M2" % Test,
      "org.scalacheck"             %% "scalacheck"   % "1.14.3" % Test,
      "com.github.julien-truffaut" %% "monocle-core" % "2.0.0",
      "org.apache.commons"         % "commons-lang3" % "3.9"
    ),
    libraryDependencies ++= Seq(
      compilerPlugin(
        "com.github.ghik" % "silencer-plugin" % silencerVersion cross CrossVersion.full
      ),
      "com.github.ghik" % "silencer-lib" % silencerVersion % Provided cross CrossVersion.full
    ),
    addCompilerPlugin(
      "org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full
    ),
    addCompilerPlugin("io.tryp" % "splain" % "0.5.1" cross CrossVersion.patch),
    scalacOptions ++= Seq(
      "-P:splain:infix:true",
      "-P:splain:foundreq:true",
      "-P:splain:implicits:true",
      "-P:splain:tree:true"
    )
  )

lazy val pkg = (project in file("common/pkg"))
  .settings(
    mainClass in (Compile, run) := Some("org.enso.pkg.Main"),
    version := "0.1",
    libraryDependencies ++= circe ++ Seq(
      "io.circe"   %% "circe-yaml" % "0.12.0", // separate from other circe deps because its independent project with its own versioning
      "commons-io" % "commons-io"  % "2.6"
    )
  )

lazy val file_manager = (project in file("common/file-manager"))
  .settings(
    (Compile / mainClass) := Some("org.enso.filemanager.FileManager")
  )
  .settings(
    libraryDependencies ++= akka,
    libraryDependencies += akkaSLF4J,
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3",
    libraryDependencies += "org.scalatest"  %% "scalatest"      % "3.2.0-M2" % Test,
    libraryDependencies += "org.scalacheck" %% "scalacheck"     % "1.14.3" % Test,
    libraryDependencies += akkaTestkitTyped,
    libraryDependencies += "commons-io" % "commons-io" % "2.6",
    // upgrade blocked by gmethvin/directory-watcher#49
    libraryDependencies += "io.methvin" % "directory-watcher" % "0.9.6"
  )

lazy val project_manager = (project in file("common/project-manager"))
  .settings(
    (Compile / mainClass) := Some("org.enso.projectmanager.boot.ProjectManager")
  )
  .settings(
    (Compile / run / fork) := true,
    (Test / fork) := true,
    (Compile / run / connectInput) := true,
    javaOptions ++= Seq(
      // Puts the language runtime on the truffle classpath, rather than the
      // standard classpath. This is the recommended way of handling this and
      // we should strive to use such structure everywhere. See
      // https://www.graalvm.org/docs/graalvm-as-a-platform/implement-language#graalvm
      s"-Dtruffle.class.path.append=${(runtime / Compile / fullClasspath).value
        .map(_.data)
        .mkString(File.pathSeparator)}"
    ),
    libraryDependencies ++= akka,
    libraryDependencies ++= circe,
    libraryDependencies ++= Seq(
      "com.typesafe"               % "config"               % "1.4.0",
      "com.github.pureconfig"      %% "pureconfig"          % "0.12.2",
      "ch.qos.logback"             % "logback-classic"      % "1.2.3",
      "com.typesafe.scala-logging" %% "scala-logging"       % "3.9.2",
      "dev.zio"                    %% "zio"                 % "1.0.0-RC18-2",
      "dev.zio"                    %% "zio-interop-cats"    % "2.0.0.0-RC12",
      "commons-io"                 % "commons-io"           % "2.6",
      "com.beachape"               %% "enumeratum-circe"    % "1.5.23",
      "com.miguno.akka"            %% "akka-mock-scheduler" % "0.5.5" % Test,
      "org.mockito"                %% "mockito-scala"       % "1.13.7" % Test
    ),
    addCompilerPlugin(
      "org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full
    )
  )
  .dependsOn(pkg)
  .dependsOn(language_server)
  .dependsOn(`json-rpc-server`)
  .dependsOn(`json-rpc-server-test` % Test)

//////////////////////
//// Sub Projects ////
//////////////////////

val truffleRunOptions = Seq(
  "-Dpolyglot.engine.IterativePartialEscape=true",
  "-XX:-UseJVMCIClassLoader",
  "-Dpolyglot.engine.BackgroundCompilation=false",
  "-Dgraalvm.locatorDisabled=true"
)

val truffleRunOptionsSettings = Seq(
  fork := true,
  javaOptions ++= truffleRunOptions
)

lazy val core_definition = (project in file("engine/core-definition"))
  .configs(Benchmark)
  .settings(
    version := "0.1",
    inConfig(Compile)(truffleRunOptionsSettings),
    inConfig(Benchmark)(Defaults.testSettings),
    parallelExecution in Test := false,
    logBuffered in Test := false,
    scalacOptions += "-Ymacro-annotations",
    libraryDependencies ++= jmh ++ Seq(
      "com.chuusai"                %% "shapeless"    % "2.3.3",
      "org.scalacheck"             %% "scalacheck"   % "1.14.3" % Test,
      "org.scalactic"              %% "scalactic"    % "3.2.0-M2" % Test,
      "org.scalatest"              %% "scalatest"    % "3.2.0-M2" % Test,
      "org.typelevel"              %% "cats-core"    % catsVersion,
      "com.github.julien-truffaut" %% "monocle-core" % "2.0.0"
    ),
    addCompilerPlugin(
      "org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full
    ),
    addCompilerPlugin("io.tryp" % "splain" % "0.5.1" cross CrossVersion.patch),
    scalacOptions ++= Seq(
      "-P:splain:infix:true",
      "-P:splain:foundreq:true",
      "-P:splain:implicits:true",
      "-P:splain:tree:true"
    )
  )
  .dependsOn(graph)
  .dependsOn(syntax.jvm)

lazy val polyglot_api = project
  .in(file("engine/polyglot-api"))
  .settings(
    Test / fork := true,
    Test / javaOptions ++= Seq(
      // Puts the language runtime on the truffle classpath, rather than the
      // standard classpath. This is the recommended way of handling this and
      // we should strive to use such structure everywhere. See
      // https://www.graalvm.org/docs/graalvm-as-a-platform/implement-language#graalvm
      s"-Dtruffle.class.path.append=${(LocalProject("runtime") / Compile / fullClasspath).value
        .map(_.data)
        .mkString(File.pathSeparator)}"
    ),
    libraryDependencies ++= Seq(
      "org.graalvm.sdk"                  % "polyglot-tck"            % graalVersion % "provided",
      "org.scalatest"                    %% "scalatest"              % "3.2.0-M2" % Test,
      "org.scalacheck"                   %% "scalacheck"             % "1.14.3" % Test,
      "com.fasterxml.jackson.dataformat" % "jackson-dataformat-cbor" % "2.10.3",
      "com.fasterxml.jackson.core"       % "jackson-databind"        % "2.10.3",
      "com.fasterxml.jackson.module"     %% "jackson-module-scala"   % "2.10.3"
    ),
    addCompilerPlugin(
      "org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full
    ),
    addCompilerPlugin("io.tryp" % "splain" % "0.5.1" cross CrossVersion.patch),
    scalacOptions ++= Seq(
      "-P:splain:infix:true",
      "-P:splain:foundreq:true",
      "-P:splain:implicits:true",
      "-P:splain:tree:true"
    )
  )
  .dependsOn(pkg)

lazy val language_server = (project in file("engine/language-server"))
  .settings(
    libraryDependencies ++= akka ++ circe ++ Seq(
      "ch.qos.logback"             % "logback-classic" % "1.2.3",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
      "io.circe"                   %% "circe-generic-extras" % "0.12.2",
      "io.circe"                   %% "circe-literal" % circeVersion,
      "org.bouncycastle"           % "bcpkix-jdk15on" % "1.64",
      "dev.zio"                    %% "zio" % "1.0.0-RC18-2",
      "io.methvin"                 % "directory-watcher" % "0.9.6",
      "com.beachape"               %% "enumeratum-circe" % "1.5.23",
      akkaTestkit                  % Test,
      "commons-io"                 % "commons-io" % "2.6",
      "org.scalatest"              %% "scalatest" % "3.2.0-M2" % Test,
      "org.scalacheck"             %% "scalacheck" % "1.14.0" % Test,
      "org.graalvm.sdk"            % "polyglot-tck" % graalVersion % "provided"
    ),
    testOptions in Test += Tests
      .Argument(TestFrameworks.ScalaCheck, "-minSuccessfulTests", "1000")
  )
  .configs(Benchmark)
  .settings(
    inConfig(Benchmark)(Defaults.testSettings),
    bench := (test in Benchmark).value,
    libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.19" % "bench",
    testFrameworks ++= List(
      new TestFramework("org.scalameter.ScalaMeterFramework")
    )
  )
  .dependsOn(polyglot_api)
  .dependsOn(`json-rpc-server`)
  .dependsOn(`json-rpc-server-test` % Test)

lazy val runtime = (project in file("engine/runtime"))
  .configs(Benchmark)
  .settings(
    version := "0.1",
    commands += WithDebugCommand.withDebug,
    inConfig(Compile)(truffleRunOptionsSettings),
    inConfig(Benchmark)(Defaults.testSettings),
    parallelExecution in Test := false,
    logBuffered in Test := false,
    scalacOptions += "-Ymacro-annotations",
    scalacOptions ++= Seq("-Ypatmat-exhaust-depth", "off"),
    libraryDependencies ++= jmh ++ Seq(
      "com.chuusai"         %% "shapeless"            % "2.3.3",
      "org.apache.commons"  % "commons-lang3"         % "3.9",
      "org.apache.tika"     % "tika-core"             % "1.23",
      "org.graalvm.sdk"     % "graal-sdk"             % graalVersion % "provided",
      "org.graalvm.sdk"     % "polyglot-tck"          % graalVersion % "provided",
      "org.graalvm.truffle" % "truffle-api"           % graalVersion % "provided",
      "org.graalvm.truffle" % "truffle-dsl-processor" % graalVersion % "provided",
      "org.graalvm.truffle" % "truffle-tck"           % graalVersion % "provided",
      "org.graalvm.truffle" % "truffle-tck-common"    % graalVersion % "provided",
      "org.scalacheck"      %% "scalacheck"           % "1.14.3" % Test,
      "org.scalactic"       %% "scalactic"            % "3.2.0-M2" % Test,
      "org.scalatest"       %% "scalatest"            % "3.2.0-M2" % Test,
      "org.graalvm.truffle" % "truffle-api"           % graalVersion % Benchmark,
      "org.typelevel"       %% "cats-core"            % catsVersion,
      "eu.timepit"          %% "refined"              % "0.9.12"
    ),
    // Note [Unmanaged Classpath]
    Compile / unmanagedClasspath += (core_definition / Compile / packageBin).value,
    Test / unmanagedClasspath += (core_definition / Compile / packageBin).value,
    Compile / compile := (Compile / compile)
      .dependsOn(core_definition / Compile / packageBin)
      .value
  )
  .settings(
    (Compile / javacOptions) ++= Seq(
      "-s",
      (Compile / sourceManaged).value.getAbsolutePath
    ),
    addCompilerPlugin(
      "org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full
    ),
    addCompilerPlugin("io.tryp" % "splain" % "0.5.1" cross CrossVersion.patch),
    scalacOptions ++= Seq(
      "-P:splain:infix:true",
      "-P:splain:foundreq:true",
      "-P:splain:implicits:true",
      "-P:splain:tree:true"
    )
  )
  .settings(
    (Compile / compile) := (Compile / compile)
      .dependsOn(Def.task { (Compile / sourceManaged).value.mkdirs })
      .value
  )
  .settings(
    logBuffered := false,
    bench := (test in Benchmark).tag(Exclusive).value,
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
  .dependsOn(syntax.jvm)
  .dependsOn(graph)
  .dependsOn(polyglot_api)

/* Note [Unmanaged Classpath]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~
 * As the definition of the core primitives in `core_definition` is achieved
 * entirely using the graph macros, this means that the IDE experience for those
 * using these primitives is very poor.
 *
 * To get around this, we want to treat the core definition as a .jar dependency
 * to force the IDE to depend on bytecode for its diagnostics, rather than the
 * source code (as this means it sees the macros expanded). A standard workflow
 * with local publishing would not recompile the definition automatically on
 * changes, so the `unmanagedClasspath` route allows us to get automatic
 * recompilation but still convince the IDE that it is a .jar dependency.
 */

lazy val runner = project
  .in(file("engine/runner"))
  .settings(
    mainClass in (Compile, run) := Some("org.enso.runner.Main"),
    mainClass in assembly := (Compile / run / mainClass).value,
    assemblyJarName in assembly := "enso.jar",
    test in assembly := {},
    assemblyOutputPath in assembly := file("enso.jar"),
    assemblyMergeStrategy in assembly := {
      case PathList("META-INF", file, xs @ _*) if file.endsWith(".DSA") =>
        MergeStrategy.discard
      case PathList("META-INF", file, xs @ _*) if file.endsWith(".SF") =>
        MergeStrategy.discard
      case PathList("META-INF", "MANIFEST.MF", xs @ _*) =>
        MergeStrategy.discard
      case "application.conf" =>
        MergeStrategy.concat
      case "reference.conf" =>
        MergeStrategy.concat
      case x =>
        MergeStrategy.first
    },
    assemblyOption in assembly := (assemblyOption in assembly).value
      .copy(
        prependShellScript = Some(
          defaultUniversalScript(
            shebang  = false,
            javaOpts = truffleRunOptions
          )
        )
      ),
    commands += WithDebugCommand.withDebug,
    inConfig(Compile)(truffleRunOptionsSettings),
    libraryDependencies ++= Seq(
      "org.graalvm.sdk"       % "polyglot-tck"           % graalVersion % "provided",
      "org.graalvm.truffle"   % "truffle-api"            % graalVersion % "provided",
      "commons-cli"           % "commons-cli"            % "1.4",
      "io.github.spencerpark" % "jupyter-jvm-basekernel" % "2.3.0",
      "org.jline"             % "jline"                  % "3.13.3",
      "org.typelevel"         %% "cats-core"             % "2.0.0"
    ),
    connectInput in run := true
  )
  .settings(
    buildNativeImage := Def
      .task {
        val javaHome         = System.getProperty("java.home")
        val nativeImagePath  = s"$javaHome/bin/native-image"
        val classPath        = (Runtime / fullClasspath).value.files.mkString(":")
        val resourcesGlobOpt = "-H:IncludeResources=.*Main.enso$"
        val cmd =
          s"$nativeImagePath $resourcesGlobOpt --macro:truffle --no-fallback --initialize-at-build-time -cp $classPath ${(Compile / mainClass).value.get} enso"
        cmd !
      }
      .dependsOn(Compile / compile)
      .value
  )
  .settings(
    Compile / sourceGenerators += Def.task {
      val file = (Compile / sourceManaged).value / "buildinfo" / "Info.scala"
      BuildInfo
        .writeBuildInfoFile(file, ensoVersion, scalacVersion, graalVersion)
    }.taskValue
  )
  .dependsOn(runtime)
  .dependsOn(pkg)
  .dependsOn(language_server)
  .dependsOn(polyglot_api)

lazy val `json-rpc-server` = project
  .in(file("common/json-rpc-server"))
  .settings(
    libraryDependencies ++= akka,
    libraryDependencies ++= circe,
    libraryDependencies ++= Seq(
      "io.circe"      %% "circe-literal" % circeVersion,
      akkaTestkit     % Test,
      "org.scalatest" %% "scalatest" % "3.2.0-M2" % Test
    )
  )

lazy val `json-rpc-server-test` = project
  .in(file("common/json-rpc-server-test"))
  .settings(
    libraryDependencies ++= akka,
    libraryDependencies ++= circe,
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-literal" % circeVersion,
      akkaTestkit,
      "org.scalatest" %% "scalatest" % "3.2.0-M2"
    )
  )
  .dependsOn(`json-rpc-server`)
