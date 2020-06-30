import java.io.File

import org.enso.build.BenchTasks._
import org.enso.build.WithDebugCommand
import sbt.Keys.scalacOptions
import sbt.addCompilerPlugin
import sbtassembly.AssemblyPlugin.defaultUniversalScript
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

import scala.sys.process._

// ============================================================================
// === Global Configuration ===================================================
// ============================================================================

val scalacVersion = "2.13.3"
val graalVersion  = "20.1.0"
val ensoVersion   = "0.0.1"
organization in ThisBuild := "org.enso"
scalaVersion in ThisBuild := scalacVersion
val coursierCache = file("~/.cache/coursier/v1")

Global / onChangedBuildSource := ReloadOnSourceChanges

// ============================================================================
// === Compiler Options =======================================================
// ============================================================================

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
    "-Xfatal-warnings"                    // Make warnings fatal so they don't make it onto main (use @nowarn for local suppression)
  )

val jsSettings = Seq(
  scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) }
)

scalacOptions in (Compile, console) ~= (_ filterNot (_ == "-Xfatal-warnings"))

// ============================================================================
// === Benchmark Configuration ================================================
// ============================================================================

lazy val Benchmark = config("bench") extend sbt.Test

// Native Image Generation
lazy val buildNativeImage =
  taskKey[Unit]("Build native image for the Enso executable")

// ============================================================================
// === Global Project =========================================================
// ============================================================================

lazy val enso = (project in file("."))
  .settings(version := "0.1")
  .aggregate(
    `core-definition`,
    `interpreter-dsl`,
    `json-rpc-server-test`,
    `json-rpc-server`,
    `language-server`,
    `parser-service`,
    `polyglot-api`,
    `project-manager`,
    `syntax-definition`.jvm,
    `text-buffer`,
    flexer.jvm,
    graph,
    logger.jvm,
    pkg,
    runner,
    runtime,
    searcher,
    syntax.jvm
  )
  .settings(Global / concurrentRestrictions += Tags.exclusive(Exclusive))

// ============================================================================
// === Dependency Versions ====================================================
// ============================================================================

/* Note [Dependency Versions]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Please maintain the following section in alphabetical order for the bundles
 * of dependencies. Additionally, please keep the 'Other' subsection in
 * alphabetical order.
 *
 * Furthermore, please keep the following in mind:
 * - Wherever possible, we should use the same version of a dependency
 *   throughout the project.
 * - If you need to include a new dependency, please define its version in this
 *   section.
 * - If that version is not the latest, please include a note explaining why
 *   this is the case.
 * - If, for some reason, you need to use a dependency version other than the
 *   global one, please include a note explaining why this is the case, and the
 *   circumstances under which the dependency could be upgraded to use the
 *   global version
 */

// === Akka ===================================================================

def akkaPkg(name: String)     = akkaURL                       %% s"akka-$name" % akkaVersion
def akkaHTTPPkg(name: String) = akkaURL                       %% s"akka-$name" % akkaHTTPVersion
val akkaURL                   = "com.typesafe.akka"
val akkaVersion               = "2.6.6"
val akkaHTTPVersion           = "10.2.0-RC1"
val akkaMockSchedulerVersion  = "0.5.5"
val akkaActor                 = akkaPkg("actor")
val akkaStream                = akkaPkg("stream")
val akkaTyped                 = akkaPkg("actor-typed")
val akkaTestkit               = akkaPkg("testkit")
val akkaSLF4J                 = akkaPkg("slf4j")
val akkaTestkitTyped          = akkaPkg("actor-testkit-typed") % Test
val akkaHttp                  = akkaHTTPPkg("http")
val akkaSpray                 = akkaHTTPPkg("http-spray-json")
val akka =
  Seq(akkaActor, akkaStream, akkaHttp, akkaSpray, akkaTyped)

// === Cats ===================================================================

val catsVersion    = "2.2.0-M3"
val kittensVersion = "2.1.0"
val cats = {
  Seq(
    "org.typelevel" %% "cats-core"   % catsVersion,
    "org.typelevel" %% "cats-effect" % catsVersion,
    "org.typelevel" %% "cats-free"   % catsVersion,
    "org.typelevel" %% "cats-macros" % catsVersion,
    "org.typelevel" %% "kittens"     % kittensVersion
  )
}

// === Circe ==================================================================

val circeVersion              = "0.14.0-M1"
val circeYamlVersion          = "0.13.1"
val enumeratumCirceVersion    = "1.6.1"
val circeGenericExtrasVersion = "0.13.0"
val circe = Seq("circe-core", "circe-generic", "circe-parser")
  .map("io.circe" %% _ % circeVersion)

// === Commons ================================================================

val commonsCollectionsVersion = "4.4"
val commonsLangVersion        = "3.10"
val commonsIoVersion          = "2.7"
val commonsTextVersion        = "1.8"
val commonsMathVersion        = "3.6.1"
val commonsCompressVersion    = "1.20"
val commonsCliVersion         = "1.4"
val commons = Seq(
  "org.apache.commons" % "commons-collections4" % commonsCollectionsVersion,
  "org.apache.commons" % "commons-lang3"        % commonsLangVersion,
  "commons-io"         % "commons-io"           % commonsIoVersion,
  "org.apache.commons" % "commons-text"         % commonsTextVersion,
  "org.apache.commons" % "commons-math3"        % commonsMathVersion,
  "commons-cli"        % "commons-cli"          % commonsCliVersion
)

// === Jackson ================================================================

val jacksonVersion = "2.11.1"
val jackson = Seq(
  "com.fasterxml.jackson.dataformat" % "jackson-dataformat-cbor" % jacksonVersion,
  "com.fasterxml.jackson.core"       % "jackson-databind"        % jacksonVersion,
  "com.fasterxml.jackson.module"    %% "jackson-module-scala"    % jacksonVersion
)

// === JMH ====================================================================

val jmhVersion = "1.23"
val jmh = Seq(
  "org.openjdk.jmh" % "jmh-core"                 % jmhVersion % Benchmark,
  "org.openjdk.jmh" % "jmh-generator-annprocess" % jmhVersion % Benchmark
)

// === Monocle ================================================================

val monocleVersion = "2.0.5"
val monocle = {
  Seq(
    "com.github.julien-truffaut" %% "monocle-core"  % monocleVersion,
    "com.github.julien-truffaut" %% "monocle-macro" % monocleVersion,
    "com.github.julien-truffaut" %% "monocle-law"   % monocleVersion % "test"
  )
}

// === Scala Compiler =========================================================

val scalaCompiler = Seq(
  "org.scala-lang" % "scala-reflect"  % scalacVersion,
  "org.scala-lang" % "scala-compiler" % scalacVersion
)

// === Splain =================================================================

val splainVersion = "0.5.7"
val splainOptions = Seq(
  "-P:splain:infix:true",
  "-P:splain:foundreq:true",
  "-P:splain:implicits:true",
  "-P:splain:tree:true"
)

// === ZIO ====================================================================

val zioVersion            = "1.0.0-RC18-2"
val zioInteropCatsVersion = "2.0.0.0-RC13"
val zio = Seq(
  "dev.zio" %% "zio"              % zioVersion,
  "dev.zio" %% "zio-interop-cats" % zioInteropCatsVersion
)

// === Other ==================================================================

val bcpkixJdk15Version          = "1.65"
val declineVersion              = "1.2.0"
val directoryWatcherVersion     = "0.9.10"
val flatbuffersVersion          = "1.12.0"
val guavaVersion                = "29.0-jre"
val jlineVersion                = "3.15.0"
val jupyterJvmBasekernelVersion = "2.3.0"
val kindProjectorVersion        = "0.11.0"
val logbackClassicVersion       = "1.2.3"
val mockitoScalaVersion         = "1.14.8"
val newtypeVersion              = "0.4.4"
val pprintVersion               = "0.5.9"
val pureconfigVersion           = "0.13.0"
val refinedVersion              = "0.9.14"
val scalacheckVersion           = "1.14.3"
val scalacticVersion            = "3.3.0-SNAP2"
val scalaLoggingVersion         = "3.9.2"
val scalameterVersion           = "0.19"
val scalatagsVersion            = "0.9.1"
val scalatestVersion            = "3.3.0-SNAP2"
val shapelessVersion            = "2.4.0-M1"
val slickVersion                = "3.3.2"
val sqliteVersion               = "3.31.1"
val tikaVersion                 = "1.24.1"
val typesafeConfigVersion       = "1.4.0"

// ============================================================================
// === Internal Libraries =====================================================
// ============================================================================

lazy val logger = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("lib/logger"))
  .settings(
    version := "0.1",
    libraryDependencies ++= scalaCompiler
  )
  .jsSettings(jsSettings)

lazy val flexer = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("lib/flexer"))
  .dependsOn(logger)
  .settings(
    version := "0.1",
    resolvers += Resolver.sonatypeRepo("releases"),
    libraryDependencies ++= scalaCompiler ++ Seq(
        "com.google.guava" % "guava"     % guavaVersion,
        "org.typelevel"  %%% "cats-core" % catsVersion,
        "org.typelevel"  %%% "kittens"   % kittensVersion
      )
  )
  .jsSettings(jsSettings)

lazy val `syntax-definition` = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("lib/syntax/definition"))
  .dependsOn(logger, flexer)
  .settings(
    scalacOptions ++= Seq("-Ypatmat-exhaust-depth", "off"),
    libraryDependencies ++= monocle ++ scalaCompiler ++ Seq(
        "org.typelevel" %%% "cats-core"     % catsVersion,
        "org.typelevel" %%% "kittens"       % kittensVersion,
        "com.lihaoyi"   %%% "scalatags"     % scalatagsVersion,
        "io.circe"      %%% "circe-core"    % circeVersion,
        "io.circe"      %%% "circe-generic" % circeVersion,
        "io.circe"      %%% "circe-parser"  % circeVersion
      )
  )
  .jsSettings(jsSettings)

lazy val syntax = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("lib/syntax/specialization"))
  .dependsOn(logger, flexer, `syntax-definition`)
  .configs(Test)
  .configs(Benchmark)
  .settings(
    testFrameworks := Nil,
    scalacOptions ++= Seq("-Ypatmat-exhaust-depth", "off"),
    mainClass in (Compile, run) := Some("org.enso.syntax.text.Main"),
    version := "0.1",
    logBuffered := false,
    libraryDependencies ++= Seq(
        "org.scalatest" %%% "scalatest"     % scalatestVersion % Test,
        "com.lihaoyi"   %%% "pprint"        % pprintVersion,
        "io.circe"      %%% "circe-core"    % circeVersion,
        "io.circe"      %%% "circe-generic" % circeVersion,
        "io.circe"      %%% "circe-parser"  % circeVersion
      ),
    compile := (Compile / compile)
        .dependsOn(Def.taskDyn {
          val parserCompile =
            (`syntax-definition`.jvm / Compile / compileIncremental).value
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
    libraryDependencies +=
      "com.storm-enroute" %% "scalameter" % scalameterVersion % "bench",
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

lazy val `parser-service` = (project in file("lib/parser-service"))
  .dependsOn(syntax.jvm)
  .settings(
    libraryDependencies ++= akka,
    mainClass := Some("org.enso.ParserServiceMain")
  )

lazy val `text-buffer` = project
  .in(file("lib/text-buffer"))
  .configs(Test)
  .settings(
    libraryDependencies ++= Seq(
        "org.typelevel"  %% "cats-core"  % catsVersion,
        "org.scalatest"  %% "scalatest"  % scalatestVersion  % Test,
        "org.scalacheck" %% "scalacheck" % scalacheckVersion % Test
      )
  )

lazy val graph = (project in file("lib/graph/"))
  .dependsOn(logger.jvm)
  .configs(Test)
  .settings(
    version := "0.1",
    resolvers ++= Seq(
        Resolver.sonatypeRepo("releases"),
        Resolver.sonatypeRepo("snapshots")
      ),
    scalacOptions += "-Ymacro-annotations",
    libraryDependencies ++= scalaCompiler ++ Seq(
        "com.chuusai"                %% "shapeless"     % shapelessVersion,
        "io.estatico"                %% "newtype"       % newtypeVersion,
        "org.scalatest"              %% "scalatest"     % scalatestVersion  % Test,
        "org.scalacheck"             %% "scalacheck"    % scalacheckVersion % Test,
        "com.github.julien-truffaut" %% "monocle-core"  % monocleVersion,
        "org.apache.commons"          % "commons-lang3" % commonsLangVersion
      ),
    addCompilerPlugin(
      "org.typelevel" %% "kind-projector" % kindProjectorVersion cross CrossVersion.full
    ),
    addCompilerPlugin(
      "io.tryp" % "splain" % splainVersion cross CrossVersion.patch
    ),
    scalacOptions ++= splainOptions
  )

lazy val pkg = (project in file("lib/pkg"))
  .settings(
    mainClass in (Compile, run) := Some("org.enso.pkg.Main"),
    version := "0.1",
    libraryDependencies ++= circe ++ Seq(
        "io.circe"  %% "circe-yaml" % circeYamlVersion, // separate from other circe deps because its independent project with its own versioning
        "commons-io" % "commons-io" % commonsIoVersion
      )
  )

lazy val `project-manager` = (project in file("lib/project-manager"))
  .settings(
    (Compile / mainClass) := Some("org.enso.projectmanager.boot.ProjectManager")
  )
  .settings(
    (Compile / run / fork) := true,
    (Test / fork) := true,
    (Compile / run / connectInput) := true,
    javaOptions ++= {
      // Note [Classpath Separation]
      val runtimeClasspath =
        (runtime / Compile / fullClasspath).value
          .map(_.data)
          .mkString(File.pathSeparator)
      Seq(s"-Dtruffle.class.path.append=$runtimeClasspath")
    },
    libraryDependencies ++= akka,
    libraryDependencies ++= circe,
    libraryDependencies ++= Seq(
        "com.typesafe"                % "config"              % typesafeConfigVersion,
        "com.github.pureconfig"      %% "pureconfig"          % pureconfigVersion,
        "ch.qos.logback"              % "logback-classic"     % logbackClassicVersion,
        "com.typesafe.scala-logging" %% "scala-logging"       % scalaLoggingVersion,
        "dev.zio"                    %% "zio"                 % zioVersion,
        "dev.zio"                    %% "zio-interop-cats"    % zioInteropCatsVersion,
        "commons-io"                  % "commons-io"          % commonsIoVersion,
        "com.beachape"               %% "enumeratum-circe"    % enumeratumCirceVersion,
        "com.miguno.akka"            %% "akka-mock-scheduler" % akkaMockSchedulerVersion % Test,
        "org.mockito"                %% "mockito-scala"       % mockitoScalaVersion      % Test
      ),
    addCompilerPlugin(
      "org.typelevel" %% "kind-projector" % kindProjectorVersion cross CrossVersion.full
    )
  )
  .settings(
    assemblyJarName in assembly := "project-manager.jar",
    test in assembly := {},
    assemblyOutputPath in assembly := file("project-manager.jar"),
    assemblyMergeStrategy in assembly := {
      case PathList("META-INF", file, xs @ _*) if file.endsWith(".DSA") =>
        MergeStrategy.discard
      case PathList("META-INF", file, xs @ _*) if file.endsWith(".SF") =>
        MergeStrategy.discard
      case PathList("META-INF", "MANIFEST.MF", xs @ _*) =>
        MergeStrategy.discard
      case "application.conf" => MergeStrategy.concat
      case "reference.conf"   => MergeStrategy.concat
      case _                  => MergeStrategy.first
    },
    assemblyOption in assembly := (assemblyOption in assembly).value
        .copy(
          prependShellScript = Some(
            defaultUniversalScript(
              shebang  = false,
              javaOpts = Seq("-Dtruffle.class.path.append=runtime.jar")
            )
          )
        ),
    assembly := assembly
        .dependsOn(runtime / assembly)
        .value
  )
  .dependsOn(pkg)
  .dependsOn(`language-server`)
  .dependsOn(`json-rpc-server`)
  .dependsOn(`json-rpc-server-test` % Test)

/* Note [Classpath Separation]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Projects using the language runtime do not depend on it directly, but instead
 * the language runtime is put on the Truffle classpath, rather than the
 * standard classpath. This is the recommended way of handling this and we
 * strive to use such structure everywhere.
 * See
 * https://www.graalvm.org/docs/graalvm-as-a-platform/implement-language#graalvm
 *
 * Currently the only exception to this are the tests of the runtime project
 * which have classpath separation disabled, because they need direct access to
 * the runtime's instruments.
 *
 * To ensure correct handling of dependencies by sbt, the classpath appended to
 * Java options, should be based on `(runtime / Compile / fullClasspath).value`
 * wherever possible. Using a key from the runtime project enables sbt to see
 * the dependency.
 *
 * Assembly tasks that build JAR files which need `runtime.jar` to run should
 * also add a dependency on `runtime / assembly`.
 */

lazy val `json-rpc-server` = project
  .in(file("lib/json-rpc-server"))
  .settings(
    libraryDependencies ++= akka,
    libraryDependencies ++= circe,
    libraryDependencies ++= Seq(
        "io.circe"      %% "circe-literal" % circeVersion,
        akkaTestkit      % Test,
        "org.scalatest" %% "scalatest"     % scalatestVersion % Test
      )
  )

lazy val `json-rpc-server-test` = project
  .in(file("lib/json-rpc-server-test"))
  .settings(
    libraryDependencies ++= akka,
    libraryDependencies ++= circe,
    libraryDependencies ++= Seq(
        "io.circe" %% "circe-literal" % circeVersion,
        akkaTestkit,
        "org.scalatest" %% "scalatest" % scalatestVersion
      )
  )
  .dependsOn(`json-rpc-server`)

lazy val `core-definition` = (project in file("lib/core-definition"))
  .configs(Benchmark)
  .settings(
    version := "0.1",
    inConfig(Compile)(truffleRunOptionsSettings),
    inConfig(Benchmark)(Defaults.testSettings),
    parallelExecution in Test := false,
    logBuffered in Test := false,
    scalacOptions += "-Ymacro-annotations",
    libraryDependencies ++= jmh ++ Seq(
        "com.chuusai"                %% "shapeless"    % shapelessVersion,
        "org.scalacheck"             %% "scalacheck"   % scalacheckVersion % Test,
        "org.scalactic"              %% "scalactic"    % scalacticVersion  % Test,
        "org.scalatest"              %% "scalatest"    % scalatestVersion  % Test,
        "org.typelevel"              %% "cats-core"    % catsVersion,
        "com.github.julien-truffaut" %% "monocle-core" % monocleVersion
      ),
    addCompilerPlugin(
      "org.typelevel" %% "kind-projector" % kindProjectorVersion cross CrossVersion.full
    ),
    addCompilerPlugin(
      "io.tryp" % "splain" % splainVersion cross CrossVersion.patch
    ),
    scalacOptions ++= splainOptions
  )
  .dependsOn(graph)
  .dependsOn(syntax.jvm)

lazy val searcher = project
  .in(file("lib/searcher"))
  .configs(Test)
  .settings(
    libraryDependencies ++= Seq(
        "com.typesafe.slick" %% "slick"       % slickVersion,
        "org.xerial"          % "sqlite-jdbc" % sqliteVersion,
        "org.scalatest"      %% "scalatest"   % scalatestVersion % Test
      )
  )

// ============================================================================
// === Sub-Projects ===========================================================
// ============================================================================

val truffleRunOptions = Seq(
  "-Dpolyglot.engine.IterativePartialEscape=true",
  "-Dpolyglot.engine.BackgroundCompilation=false"
)

val truffleRunOptionsSettings = Seq(
  fork := true,
  javaOptions ++= truffleRunOptions
)

lazy val `polyglot-api` = project
  .in(file("engine/polyglot-api"))
  .settings(
    Test / fork := true,
    Test / javaOptions ++= {
      // Note [Classpath Separation]
      val runtimeClasspath =
        (LocalProject("runtime") / Compile / fullClasspath).value
          .map(_.data)
          .mkString(File.pathSeparator)
      Seq(s"-Dtruffle.class.path.append=$runtimeClasspath")
    },
    libraryDependencies ++= Seq(
        "org.graalvm.sdk"        % "polyglot-tck"     % graalVersion      % "provided",
        "com.google.flatbuffers" % "flatbuffers-java" % flatbuffersVersion,
        "org.scalatest"         %% "scalatest"        % scalatestVersion  % Test,
        "org.scalacheck"        %% "scalacheck"       % scalacheckVersion % Test
      ),
    libraryDependencies ++= jackson,
    addCompilerPlugin(
      "org.typelevel" %% "kind-projector" % kindProjectorVersion cross CrossVersion.full
    ),
    addCompilerPlugin(
      "io.tryp" % "splain" % splainVersion cross CrossVersion.patch
    ),
    scalacOptions ++= splainOptions,
    GenerateFlatbuffers.flatcVersion := flatbuffersVersion,
    sourceGenerators in Compile += GenerateFlatbuffers.task
  )
  .dependsOn(pkg)
  .dependsOn(`text-buffer`)
  .dependsOn(`searcher`)

lazy val `language-server` = (project in file("engine/language-server"))
  .settings(
    libraryDependencies ++= akka ++ circe ++ Seq(
        "ch.qos.logback"              % "logback-classic"      % logbackClassicVersion,
        "com.typesafe.scala-logging" %% "scala-logging"        % scalaLoggingVersion,
        "io.circe"                   %% "circe-generic-extras" % circeGenericExtrasVersion,
        "io.circe"                   %% "circe-literal"        % circeVersion,
        "org.bouncycastle"            % "bcpkix-jdk15on"       % bcpkixJdk15Version,
        "dev.zio"                    %% "zio"                  % zioVersion,
        "io.methvin"                  % "directory-watcher"    % directoryWatcherVersion,
        "com.beachape"               %% "enumeratum-circe"     % enumeratumCirceVersion,
        "com.google.flatbuffers"      % "flatbuffers-java"     % flatbuffersVersion,
        akkaTestkit                   % Test,
        "commons-io"                  % "commons-io"           % commonsIoVersion,
        "org.scalatest"              %% "scalatest"            % scalatestVersion  % Test,
        "org.scalacheck"             %% "scalacheck"           % scalacheckVersion % Test,
        "org.graalvm.sdk"             % "polyglot-tck"         % graalVersion      % "provided"
      ),
    testOptions in Test += Tests
        .Argument(TestFrameworks.ScalaCheck, "-minSuccessfulTests", "1000"),
    GenerateFlatbuffers.flatcVersion := flatbuffersVersion,
    sourceGenerators in Compile += GenerateFlatbuffers.task
  )
  .configs(Benchmark)
  .settings(
    inConfig(Benchmark)(Defaults.testSettings),
    bench := (test in Benchmark).value,
    libraryDependencies += "com.storm-enroute" %% "scalameter" % scalameterVersion % "bench",
    testFrameworks ++= List(
        new TestFramework("org.scalameter.ScalaMeterFramework")
      )
  )
  .dependsOn(`polyglot-api`)
  .dependsOn(`json-rpc-server`)
  .dependsOn(`json-rpc-server-test` % Test)
  .dependsOn(`text-buffer`)
  .dependsOn(`searcher`)

lazy val runtime = (project in file("engine/runtime"))
  .configs(Benchmark)
  .settings(
    version := ensoVersion,
    commands += WithDebugCommand.withDebug,
    inConfig(Compile)(truffleRunOptionsSettings),
    inConfig(Benchmark)(Defaults.testSettings),
    parallelExecution in Test := false,
    logBuffered in Test := false,
    scalacOptions += "-Ymacro-annotations",
    scalacOptions ++= Seq("-Ypatmat-exhaust-depth", "off"),
    libraryDependencies ++= circe ++ jmh ++ Seq(
        "com.chuusai"        %% "shapeless"             % shapelessVersion,
        "org.apache.commons"  % "commons-lang3"         % commonsLangVersion,
        "org.apache.tika"     % "tika-core"             % tikaVersion,
        "org.graalvm.sdk"     % "graal-sdk"             % graalVersion      % "provided",
        "org.graalvm.sdk"     % "polyglot-tck"          % graalVersion      % "provided",
        "org.graalvm.truffle" % "truffle-api"           % graalVersion      % "provided",
        "org.graalvm.truffle" % "truffle-dsl-processor" % graalVersion      % "provided",
        "org.graalvm.truffle" % "truffle-tck"           % graalVersion      % "provided",
        "org.graalvm.truffle" % "truffle-tck-common"    % graalVersion      % "provided",
        "org.scalacheck"     %% "scalacheck"            % scalacheckVersion % Test,
        "org.scalactic"      %% "scalactic"             % scalacticVersion  % Test,
        "org.scalatest"      %% "scalatest"             % scalatestVersion  % Test,
        "org.graalvm.truffle" % "truffle-api"           % graalVersion      % Benchmark,
        "org.typelevel"      %% "cats-core"             % catsVersion,
        "eu.timepit"         %% "refined"               % refinedVersion
      ),
    // Note [Unmanaged Classpath]
    Compile / unmanagedClasspath += (`core-definition` / Compile / packageBin).value,
    Test / unmanagedClasspath += (`core-definition` / Compile / packageBin).value,
    Compile / compile := FixInstrumentsGeneration.patchedCompile
        .dependsOn(`core-definition` / Compile / packageBin)
        .dependsOn(FixInstrumentsGeneration.preCompileTask)
        .value,
    // Note [Classpath Separation]
    Test / javaOptions ++= Seq(
        "-XX:-UseJVMCIClassLoader",
        "-Dgraalvm.locatorDisabled=true"
      )
  )
  .settings(
    (Compile / javacOptions) ++= Seq(
        "-s",
        (Compile / sourceManaged).value.getAbsolutePath
      ),
    addCompilerPlugin(
      "org.typelevel" %% "kind-projector" % kindProjectorVersion cross CrossVersion.full
    ),
    addCompilerPlugin(
      "io.tryp" % "splain" % splainVersion cross CrossVersion.patch
    ),
    scalacOptions ++= splainOptions
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
  .settings(
    assemblyJarName in assembly := "runtime.jar",
    test in assembly := {},
    assemblyOutputPath in assembly := file("runtime.jar"),
    assemblyMergeStrategy in assembly := {
      case PathList("META-INF", file, xs @ _*) if file.endsWith(".DSA") =>
        MergeStrategy.discard
      case PathList("META-INF", file, xs @ _*) if file.endsWith(".SF") =>
        MergeStrategy.discard
      case PathList("META-INF", "MANIFEST.MF", xs @ _*) =>
        MergeStrategy.discard
      case _ => MergeStrategy.first
    }
  )
  .dependsOn(pkg)
  .dependsOn(`interpreter-dsl`)
  .dependsOn(syntax.jvm)
  .dependsOn(graph)
  .dependsOn(`polyglot-api`)
  .dependsOn(`text-buffer`)
  .dependsOn(`searcher`)

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
    javaOptions ++= {
      // Note [Classpath Separation]
      val runtimeClasspath =
        (runtime / Compile / fullClasspath).value
          .map(_.data)
          .mkString(File.pathSeparator)
      Seq(s"-Dtruffle.class.path.append=$runtimeClasspath")
    },
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
              shebang = false,
              javaOpts = truffleRunOptions ++
                Seq("-Dtruffle.class.path.append=runtime.jar")
            )
          )
        ),
    commands += WithDebugCommand.withDebug,
    inConfig(Compile)(truffleRunOptionsSettings),
    libraryDependencies ++= Seq(
        "org.graalvm.sdk"       % "polyglot-tck"           % graalVersion % "provided",
        "org.graalvm.truffle"   % "truffle-api"            % graalVersion % "provided",
        "commons-cli"           % "commons-cli"            % commonsCliVersion,
        "com.monovore"         %% "decline"                % declineVersion,
        "io.github.spencerpark" % "jupyter-jvm-basekernel" % jupyterJvmBasekernelVersion,
        "org.jline"             % "jline"                  % jlineVersion,
        "org.typelevel"        %% "cats-core"              % catsVersion
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
      }.taskValue,
    assembly := assembly
        .dependsOn(runtime / assembly)
        .value
  )
  .dependsOn(pkg)
  .dependsOn(`language-server`)
  .dependsOn(`polyglot-api`)

lazy val `interpreter-dsl` = (project in file("lib/interpreter-dsl"))
  .settings(
    version := "0.1",
    libraryDependencies += "com.google.auto.service" % "auto-service" % "1.0-rc7"
  )
