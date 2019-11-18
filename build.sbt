import sbt.Keys.scalacOptions
import scala.sys.process._
import org.enso.build.BenchTasks._
import org.enso.build.WithDebugCommand
import sbtassembly.AssemblyPlugin.defaultUniversalScript

//////////////////////////////
//// Global Configuration ////
//////////////////////////////

val scalacVersion = "2.12.10"
val graalVersion  = "19.2.0.1"
val circeVersion  = "0.11.1"
organization in ThisBuild := "org.enso"
scalaVersion in ThisBuild := scalacVersion

Global / onChangedBuildSource := ReloadOnSourceChanges

//////////////////////////
//// Compiler Options ////
//////////////////////////

javacOptions in ThisBuild ++= Seq(
  "-encoding", // Provide explicit encoding (the next line)
  "UTF-8"      // Specify character encoding used by Java source files.
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
  "-Xlint:by-name-right-associative",   // By-name parameter of right associative operator.
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
  "-Xlint:unsound-match",               // Pattern match may not be typesafe.
  "-Xmacro-settings:-logging@org.enso", // Disable the debug logging globally.
  "-Yno-adapted-args",                  // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
  "-Ypartial-unification",              // Enable partial unification (which is enabled by default in Scala 2.13).
  "-Ypartial-unification",              // Enable partial unification in type constructor inference
  "-Ywarn-dead-code",                   // Warn when dead code is identified.
  "-Ywarn-extra-implicit",              // Warn when more than one implicit parameter section is defined.
  "-Ywarn-inaccessible",                // Warn about inaccessible types in method signatures.
  "-Ywarn-infer-any",                   // Warn when a type argument is inferred to be `Any`.
  "-Ywarn-nullary-override",            // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Ywarn-nullary-unit",                // Warn when nullary methods return Unit.
  "-Ywarn-numeric-widen",               // Warn when numerics are widened.
  "-Ywarn-unused:imports",              // Warn if an import selector is not referenced.
  "-Ywarn-unused:locals",               // Warn if a local definition is unused.
  "-Ywarn-unused:patvars",              // Warn if a variable bound in a pattern is unused.
  "-Ywarn-unused:privates",             // Warn if a private member is unused.
  "-Ywarn-value-discard"                // Warn when non-Unit expression results are unused.
)

// ENABLE THIS IN Scala 2.13.1 (where import annotation.unused is available).
//  "-Xlint:type-parameter-shadow",     // A local type parameter shadows a type already in scope.
//  "-Ywarn-unused:implicits",          // Warn if an implicit parameter is unused.
//  "-Ywarn-unused:params",             // Warn if a value parameter is unused.

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
    file_manager,
    flexer,
    graph,
    pkg,
    project_manager,
    runtime,
    parser_service,
    syntax,
    syntax_definition,
    unused
  )
  .settings(Global / concurrentRestrictions += Tags.exclusive(Exclusive))

////////////////////////////
//// Dependency Bundles ////
////////////////////////////

val monocle = {
  val monocleVersion = "1.6.0"
  Seq(
    "com.github.julien-truffaut" %% "monocle-core"  % monocleVersion,
    "com.github.julien-truffaut" %% "monocle-macro" % monocleVersion,
    "com.github.julien-truffaut" %% "monocle-law"   % monocleVersion % "test"
  )
}
val cats = {
  Seq(
    "org.typelevel" %% "cats-core"   % "2.0.0",
    "org.typelevel" %% "cats-effect" % "2.0.0",
    "org.typelevel" %% "cats-free"   % "2.0.0",
    "org.typelevel" %% "cats-macros" % "2.0.0",
    "org.typelevel" %% "kittens"     % "2.0.0"
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
val akkaVersion      = "2.5.23"
val akkaHTTPVersion  = "10.1.8"
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

lazy val logger = (project in file("common/scala/logger"))
  .dependsOn(unused)
  .settings(
    version := "0.1",
    libraryDependencies ++= scala_compiler
  )

lazy val flexer = (project in file("common/scala/flexer"))
  .dependsOn(logger)
  .settings(
    version := "0.1",
    scalacOptions -= "-deprecation", // FIXME
    resolvers += Resolver.sonatypeRepo("releases"),
    libraryDependencies ++= scala_compiler ++ Seq(
      "org.feijoas" %% "mango" % "0.14"
    )
  )

lazy val unused = (project in file("common/scala/unused"))
  .settings(version := "0.1", scalacOptions += "-nowarn")

lazy val syntax_definition = (project in file("common/scala/syntax/definition"))
  .dependsOn(logger, flexer)
  .settings(
    libraryDependencies ++= monocle ++ cats ++ circe ++ scala_compiler ++ Seq(
      "com.lihaoyi" %% "scalatags" % "0.7.0"
    )
  )

lazy val graph = (project in file("common/scala/graph/"))
  .dependsOn(logger)
  .configs(Test)
  .settings(
    version := "0.1",
    scalacOptions -= "-deprecation", // FIXME
    resolvers ++= Seq(
      Resolver.sonatypeRepo("releases"),
      Resolver.sonatypeRepo("snapshots")
    ),
    libraryDependencies ++= scala_compiler ++ Seq(
      "com.chuusai"                %% "shapeless"    % "2.3.3",
      "io.estatico"                %% "newtype"      % "0.4.3",
      "org.scalatest"              %% "scalatest"    % "3.2.0-SNAP10" % Test,
      "org.scalacheck"             %% "scalacheck"   % "1.14.0" % Test,
      "com.github.julien-truffaut" %% "monocle-core" % "2.0.0"
    ),
    libraryDependencies ++= Seq(
      compilerPlugin(
        "com.github.ghik" % "silencer-plugin" % silencerVersion cross CrossVersion.full
      ),
      "com.github.ghik" % "silencer-lib" % silencerVersion % Provided cross CrossVersion.full
    ),
    addCompilerPlugin(
      "org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full
    )
  )

lazy val syntax = (project in file("common/scala/syntax/specialization"))
  .dependsOn(logger, flexer, syntax_definition)
  .configs(Test)
  .configs(Benchmark)
  .settings(
    mainClass in (Compile, run) := Some("org.enso.syntax.text.Main"),
    version := "0.1",
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    logBuffered := false,
    inConfig(Benchmark)(Defaults.testSettings),
    bench := (test in Benchmark).tag(Exclusive).value,
    parallelExecution in Benchmark := false,
    libraryDependencies ++= circe ++ Seq(
      "com.storm-enroute" %% "scalameter" % "0.17" % "bench",
      "org.scalatest"     %% "scalatest"  % "3.0.5" % Test,
      "com.lihaoyi"       %% "pprint"     % "0.5.3"
    ),
    compile := (Compile / compile)
      .dependsOn(Def.taskDyn {
        val parserCompile =
          (syntax_definition / Compile / compileIncremental).value
        if (parserCompile.hasModified) {
          Def.task {
            streams.value.log.info("Parser changed, forcing recompilation.")
            clean.value
          }
        } else Def.task {}
      })
      .value
  )

lazy val parser_service = (project in file("common/scala/parser-service"))
  .dependsOn(syntax)
  .settings(
    libraryDependencies ++= akka,
    mainClass := Some("org.enso.ParserServiceMain")
  )

lazy val pkg = (project in file("common/scala/pkg"))
  .settings(
    mainClass in (Compile, run) := Some("org.enso.pkg.Main"),
    version := "0.1",
    libraryDependencies ++= circe ++ Seq(
      "io.circe"   %% "circe-yaml" % "0.10.0", // separate from other circe deps because its independent project with its own versioning
      "commons-io" % "commons-io"  % "2.6"
    )
  )

lazy val file_manager = (project in file("common/scala/file-manager"))
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

lazy val project_manager = (project in file("common/scala/project-manager"))
  .settings(
    (Compile / mainClass) := Some("org.enso.projectmanager.Server")
  )
  .settings(
    libraryDependencies ++= akka,
    libraryDependencies ++= circe,
    libraryDependencies += "io.spray" %% "spray-json" % "1.3.5"
  )
  .dependsOn(pkg)

//////////////////////
//// Sub Projects ////
//////////////////////

val truffleRunOptions = Seq(
  "-Dgraal.TruffleIterativePartialEscape=true",
  "-XX:-UseJVMCIClassLoader",
  "-Dgraal.TruffleBackgroundCompilation=false"
)

val truffleRunOptionsSettings = Seq(
  fork := true,
  javaOptions ++= truffleRunOptions
)

lazy val runtime = (project in file("engine/runtime"))
  .settings(
    mainClass in (Compile, run) := Some("org.enso.interpreter.Main"),
    mainClass in assembly := (Compile / run / mainClass).value,
    assemblyJarName in assembly := "enso.jar",
    test in assembly := {},
    assemblyOutputPath in assembly := file("enso.jar"),
    assemblyOption in assembly := (assemblyOption in assembly).value.copy(
      prependShellScript = Some(
        defaultUniversalScript(
          shebang  = false,
          javaOpts = truffleRunOptions
        )
      )
    ),
    version := "0.1",
    commands += WithDebugCommand.withDebug,
    inConfig(Compile)(truffleRunOptionsSettings),
    inConfig(Test)(truffleRunOptionsSettings),
    parallelExecution in Test := false,
    logBuffered in Test := false,
    libraryDependencies ++= jmh ++ Seq(
      "com.chuusai"            %% "shapeless"                % "2.3.3",
      "org.apache.commons"     % "commons-lang3"             % "3.9",
      "org.apache.tika"        % "tika-core"                 % "1.21",
      "org.graalvm.sdk"        % "graal-sdk"                 % graalVersion % "provided",
      "org.graalvm.sdk"        % "polyglot-tck"              % graalVersion % "provided",
      "org.graalvm.truffle"    % "truffle-api"               % graalVersion % "provided",
      "org.graalvm.truffle"    % "truffle-dsl-processor"     % graalVersion % "provided",
      "org.graalvm.truffle"    % "truffle-tck"               % graalVersion % "provided",
      "org.graalvm.truffle"    % "truffle-tck-common"        % graalVersion % "provided",
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
  .configs(Benchmark)
  .settings(
    logBuffered := false,
    inConfig(Benchmark)(Defaults.testSettings),
    inConfig(Benchmark)(truffleRunOptionsSettings),
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
  .dependsOn(syntax)
