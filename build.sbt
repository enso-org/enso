import LibraryManifestGenerator.BundledLibrary
import org.enso.build.BenchTasks._
import org.enso.build.WithDebugCommand
import org.apache.commons.io.FileUtils
import sbt.Keys.{libraryDependencies, scalacOptions}
import sbt.addCompilerPlugin
import sbt.complete.DefaultParsers._
import sbt.complete.Parser
import sbt.nio.file.FileTreeView
import sbt.internal.util.ManagedLogger
import src.main.scala.licenses.{
  DistributionDescription,
  SBTDistributionComponent
}

import java.io.File

// ============================================================================
// === Global Configuration ===================================================
// ============================================================================

val scalacVersion = "2.13.11"
val graalVersion  = "17.0.7"
// Version used for the Graal/Truffle related Maven packages
val graalMavenPackagesVersion = "23.0.0"
val targetJavaVersion         = graalVersion.split("\\.")(0)
val defaultDevEnsoVersion     = "0.0.0-dev"
val ensoVersion = sys.env.getOrElse(
  "ENSO_VERSION",
  defaultDevEnsoVersion
) // Note [Engine And Launcher Version]
val currentEdition = sys.env.getOrElse(
  "ENSO_EDITION",
  defaultDevEnsoVersion
) // Note [Default Editions]

// Note [Stdlib Version]
val stdLibVersion       = defaultDevEnsoVersion
val targetStdlibVersion = ensoVersion

/* Note [Engine And Launcher Version]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Currently both Engine and Launcher versions are tied to each other - each new
 * releases contains the Engine and the Launcher and thus the version number is
 * shared. If the version numbers ever diverge, make sure to update the build
 * scripts at .github/workflows accordingly.
 */

/* Note [Default Editions]
 * ~~~~~~~~~~~~~~~~~~~~~~~
 * Currently, the default edition to use is inferred based on the engine
 * version. Each Enso version has an associated default edition name and the
 * `currentEdition` field specifies the default edition name for the upcoming
 * release.
 *
 * Thus the `library-manager` needs to depend on the `version-output` to get
 * this defaults from the build metadata.
 *
 * In the future we may automate generating this edition number when cutting a
 * release.
 */

/* Note [Stdlib Version]
 * ~~~~~~~~~~~~~~~~~~~~~
 * The `stdlibVersion` variable stores the version at which standard library is
 * stored within the source tree, which is currently set to a constant of
 * `0.0.0-dev`.
 *
 * When distributions are built, the library versions are updated to match the
 * current Enso version.
 */

ThisBuild / organization := "org.enso"
ThisBuild / scalaVersion := scalacVersion

/* Tag limiting the concurrent access to tools/simple-library-server in tests.
 */
val simpleLibraryServerTag = Tags.Tag("simple-library-server")
Global / concurrentRestrictions += Tags.limit(simpleLibraryServerTag, 1)

lazy val gatherLicenses =
  taskKey[Unit]("Gathers licensing information for relevant dependencies")
gatherLicenses := {
  val _ = GatherLicenses.run.value
}
lazy val verifyLicensePackages =
  taskKey[Unit](
    "Verifies if the license package has been generated, " +
    "has no warnings and is up-to-date with dependencies."
  )
verifyLicensePackages := GatherLicenses.verifyReports.value
lazy val verifyGeneratedPackage =
  inputKey[Unit](
    "Verifies if the license package in a generated distribution is " +
    "up-to-date with the one from the report."
  )
verifyGeneratedPackage := GatherLicenses.verifyGeneratedPackage.evaluated

def makeStdLibDistribution(
  name: String,
  components: Seq[SBTDistributionComponent]
): DistributionDescription =
  Distribution(
    name,
    file(s"distribution/lib/Standard/$name/$stdLibVersion/THIRD-PARTY"),
    components
  )

GatherLicenses.distributions := Seq(
  Distribution(
    "launcher",
    file("distribution/launcher/THIRD-PARTY"),
    Distribution.sbtProjects(launcher)
  ),
  Distribution(
    "engine",
    file("distribution/engine/THIRD-PARTY"),
    Distribution.sbtProjects(
      runtime,
      `engine-runner`,
      `language-server`
    )
  ),
  Distribution(
    "project-manager",
    file("distribution/project-manager/THIRD-PARTY"),
    Distribution.sbtProjects(`project-manager`)
  ),
  makeStdLibDistribution("Base", Distribution.sbtProjects(`std-base`)),
  makeStdLibDistribution(
    "Google_Api",
    Distribution.sbtProjects(`std-google-api`)
  ),
  makeStdLibDistribution("Table", Distribution.sbtProjects(`std-table`)),
  makeStdLibDistribution("Database", Distribution.sbtProjects(`std-database`)),
  makeStdLibDistribution("Image", Distribution.sbtProjects(`std-image`)),
  makeStdLibDistribution("AWS", Distribution.sbtProjects(`std-aws`))
)

GatherLicenses.licenseConfigurations := Set("compile")
GatherLicenses.configurationRoot := file("tools/legal-review")

lazy val openLegalReviewReport =
  taskKey[Unit](
    "Gathers licensing information for relevant dependencies and opens the " +
    "report in review mode in the browser."
  )
openLegalReviewReport := {
  val _ = gatherLicenses.value
  GatherLicenses.runReportServer()
}

lazy val analyzeDependency = inputKey[Unit]("...")
analyzeDependency := GatherLicenses.analyzeDependency.evaluated

val packageBuilder = new DistributionPackage.Builder(
  ensoVersion      = ensoVersion,
  graalVersion     = graalMavenPackagesVersion,
  graalJavaVersion = graalVersion,
  artifactRoot     = file("built-distribution")
)

Global / onChangedBuildSource := ReloadOnSourceChanges
Global / excludeLintKeys += logManager

// ============================================================================
// === Compiler Options =======================================================
// ============================================================================

ThisBuild / javacOptions ++= Seq(
  "-encoding",       // Provide explicit encoding (the next line)
  "UTF-8",           // Specify character encoding used by Java source files
  "-deprecation",    // Shows a description of each use or override of a deprecated member or class
  "-g",              // Include debugging information
  "-Xlint:unchecked" // Enable additional warnings
)

ThisBuild / scalacOptions ++= Seq(
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
  "-Vimplicits",                        // Prints implicit resolution chains when no implicit can be found.
  "-Vtype-diffs",                       // Prints type errors as coloured diffs between types.
  "-Xcheckinit",                        // Wrap field accessors to throw an exception on uninitialized access.
  "-Xfatal-warnings",                   // Make warnings fatal so they don't make it onto main (use @nowarn for local suppression)
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
  "-Ywarn-unused:implicits",            // Warn if an implicit parameter is unused.
  "-Ywarn-unused:imports",              // Warn if an import selector is not referenced.
  "-Ywarn-unused:locals",               // Warn if a local definition is unused.
  "-Ywarn-unused:params",               // Warn if a value parameter is unused.
  "-Ywarn-unused:patvars",              // Warn if a variable bound in a pattern is unused.
  "-Ywarn-unused:privates"              // Warn if a private member is unused.
)

ThisBuild / Test / testOptions ++=
  Seq(Tests.Argument("-oI")) ++
  sys.env
    .get("ENSO_TEST_JUNIT_DIR")
    .map { junitDir =>
      Tests.Argument(TestFrameworks.ScalaTest, "-u", junitDir)
    }

Compile / console / scalacOptions ~= (_ filterNot (_ == "-Xfatal-warnings"))

// ============================================================================
// === Benchmark Configuration ================================================
// ============================================================================

lazy val Benchmark = config("bench") extend sbt.Test

// Native Image Generation
lazy val rebuildNativeImage = taskKey[Unit]("Force to rebuild native image")
lazy val buildNativeImage =
  taskKey[Unit]("Ensure that the Native Image is built.")

// Bootstrap task
lazy val bootstrap =
  taskKey[Unit]("Prepares Truffle JARs that are required by the sbt JVM")
bootstrap := {}

// ============================================================================
// === Global Project =========================================================
// ============================================================================

lazy val enso = (project in file("."))
  .settings(version := "0.1")
  .aggregate(
    `interpreter-dsl`,
    `interpreter-dsl-test`,
    `json-rpc-server-test`,
    `json-rpc-server`,
    `language-server`,
    `polyglot-api`,
    `project-manager`,
    `syntax-definition`,
    `syntax-rust-definition`,
    `text-buffer`,
    logger,
    pkg,
    cli,
    `task-progress-notifications`,
    `profiling-utils`,
    `logging-utils`,
    `logging-config`,
    `logging-service`,
    `logging-service-logback`,
    `logging-utils-akka`,
    filewatcher,
    `logging-truffle-connector`,
    `locking-test-helper`,
    `akka-native`,
    `version-output`,
    `refactoring-utils`,
    `engine-runner`,
    runtime,
    searcher,
    launcher,
    downloader,
    `runtime-parser`,
    `runtime-language-epb`,
    `runtime-instrument-common`,
    `runtime-instrument-id-execution`,
    `runtime-instrument-repl-debugger`,
    `runtime-instrument-runtime-server`,
    `runtime-with-instruments`,
    `runtime-with-polyglot`,
    `runtime-version-manager`,
    `runtime-version-manager-test`,
    editions,
    `distribution-manager`,
    `edition-updater`,
    `edition-uploader`,
    `library-manager`,
    `library-manager-test`,
    `connected-lock-manager`,
    syntax,
    testkit,
    `common-polyglot-core-utils`,
    `std-base`,
    `std-database`,
    `std-google-api`,
    `std-image`,
    `std-table`,
    `std-aws`,
    `simple-httpbin`,
    `enso-test-java-helpers`,
    `exploratory-benchmark-java-helpers`,
    `benchmark-java-helpers`
  )
  .settings(Global / concurrentRestrictions += Tags.exclusive(Exclusive))
  .settings(
    commands ++= Seq(packageBuilder.makePackages, packageBuilder.makeBundles)
  )

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

def akkaPkg(name: String)     = akkaURL %% s"akka-$name" % akkaVersion
def akkaHTTPPkg(name: String) = akkaURL %% s"akka-$name" % akkaHTTPVersion
val akkaURL                   = "com.typesafe.akka"
val akkaVersion               = "2.6.20"
val akkaHTTPVersion           = "10.2.10"
val akkaMockSchedulerVersion  = "0.5.5"
val logbackClassicVersion     = "1.3.7"
val logbackPkg = Seq(
  "ch.qos.logback" % "logback-classic" % logbackClassicVersion,
  "ch.qos.logback" % "logback-core"    % logbackClassicVersion
)
val akkaActor        = akkaPkg("actor")
val akkaStream       = akkaPkg("stream")
val akkaTyped        = akkaPkg("actor-typed")
val akkaTestkit      = akkaPkg("testkit")
val akkaSLF4J        = akkaPkg("slf4j")
val akkaTestkitTyped = akkaPkg("actor-testkit-typed") % Test
val akkaHttp         = akkaHTTPPkg("http")
val akkaSpray        = akkaHTTPPkg("http-spray-json")
val logbackTest      = logbackPkg.map(_ % Test)
val akka =
  Seq(
    akkaActor,
    akkaStream,
    akkaHttp,
    akkaSpray,
    akkaTyped
  )

// === Cats ===================================================================

val catsVersion = "2.9.0"

// === Circe ==================================================================

val circeVersion              = "0.14.5"
val circeYamlVersion          = "0.14.2"
val enumeratumCirceVersion    = "1.7.2"
val circeGenericExtrasVersion = "0.14.2"
val circe = Seq("circe-core", "circe-generic", "circe-parser")
  .map("io.circe" %% _ % circeVersion)

// === Commons ================================================================

val commonsCollectionsVersion = "4.4"
val commonsLangVersion        = "3.12.0"
val commonsIoVersion          = "2.12.0"
val commonsTextVersion        = "1.10.0"
val commonsMathVersion        = "3.6.1"
val commonsCompressVersion    = "1.23.0"
val commonsCliVersion         = "1.5.0"
val commons = Seq(
  "org.apache.commons" % "commons-collections4" % commonsCollectionsVersion,
  "org.apache.commons" % "commons-lang3"        % commonsLangVersion,
  "commons-io"         % "commons-io"           % commonsIoVersion,
  "org.apache.commons" % "commons-text"         % commonsTextVersion,
  "org.apache.commons" % "commons-math3"        % commonsMathVersion,
  "commons-cli"        % "commons-cli"          % commonsCliVersion
)

// === Jackson ================================================================

val jacksonVersion = "2.15.2"
val jackson = Seq(
  "com.fasterxml.jackson.dataformat" % "jackson-dataformat-cbor" % jacksonVersion,
  "com.fasterxml.jackson.core"       % "jackson-databind"        % jacksonVersion,
  "com.fasterxml.jackson.module"    %% "jackson-module-scala"    % jacksonVersion
)

// === JAXB ================================================================

val jaxbVersion = "4.0.0"
val jaxb = Seq(
  "jakarta.xml.bind" % "jakarta.xml.bind-api" % jaxbVersion % Benchmark,
  "com.sun.xml.bind" % "jaxb-impl"            % jaxbVersion % Benchmark
)

// === JMH ====================================================================

val jmhVersion = "1.36"
val jmh = Seq(
  "org.openjdk.jmh" % "jmh-core"                 % jmhVersion % Benchmark,
  "org.openjdk.jmh" % "jmh-generator-annprocess" % jmhVersion % Benchmark
)

// === Scala Compiler =========================================================

val scalaCompiler = Seq(
  "org.scala-lang" % "scala-reflect"  % scalacVersion,
  "org.scala-lang" % "scala-compiler" % scalacVersion
)

// === std-lib ================================================================

val antlrVersion            = "4.13.0"
val awsJavaSdkV1Version     = "1.12.480"
val awsJavaSdkV2Version     = "2.20.78"
val icuVersion              = "73.1"
val poiOoxmlVersion         = "5.2.3"
val redshiftVersion         = "2.1.0.15"
val univocityParsersVersion = "2.9.1"
val xmlbeansVersion         = "5.1.1"

// === ZIO ====================================================================

val zioVersion            = "2.0.14"
val zioInteropCatsVersion = "23.0.0.6"
val zio = Seq(
  "dev.zio" %% "zio"              % zioVersion,
  "dev.zio" %% "zio-interop-cats" % zioInteropCatsVersion
)

// === Other ==================================================================

val bcpkixJdk15Version      = "1.70"
val bumpVersion             = "0.1.3"
val declineVersion          = "2.4.1"
val directoryWatcherVersion = "0.9.10"
val flatbuffersVersion      = "1.12.0"
val guavaVersion            = "32.0.0-jre"
val jlineVersion            = "3.23.0"
val jgitVersion             = "6.7.0.202309050840-r"
val diffsonVersion          = "4.4.0"
val kindProjectorVersion    = "0.13.2"
val mockitoScalaVersion     = "1.17.14"
val newtypeVersion          = "0.4.4"
val pprintVersion           = "0.8.1"
val pureconfigVersion       = "0.17.4"
val scalacheckVersion       = "1.17.0"
val scalacticVersion        = "3.3.0-SNAP4"
val scalaLoggingVersion     = "3.9.4"
val scalameterVersion       = "0.19"
val scalatestVersion        = "3.3.0-SNAP4"
val shapelessVersion        = "2.3.10"
val slf4jVersion            = "2.0.9"
val slickVersion            = "3.4.1"
val sqliteVersion           = "3.42.0.0"
val tikaVersion             = "2.4.1"
val typesafeConfigVersion   = "1.4.2"
val junitVersion            = "4.13.2"
val junitIfVersion          = "0.13.2"
val hamcrestVersion         = "1.3"
val netbeansApiVersion      = "RELEASE180"
val fansiVersion            = "0.4.0"
val httpComponentsVersion   = "4.4.1"

// ============================================================================
// === Internal Libraries =====================================================
// ============================================================================

lazy val logger = (project in file("lib/scala/logger"))
  .settings(
    frgaalJavaCompilerSetting,
    version := "0.1",
    libraryDependencies ++= scalaCompiler
  )

lazy val `syntax-definition` =
  project in file("lib/scala/syntax/definition")

lazy val syntax = (project in file("lib/scala/syntax/specialization"))
  .dependsOn(`syntax-definition`)
  .settings(
    commands += WithDebugCommand.withDebug,
    testFrameworks := Nil,
    scalacOptions ++= Seq("-Ypatmat-exhaust-depth", "off"),
    Compile / run / mainClass := Some("org.enso.syntax.text.Main"),
    version := "0.1",
    logBuffered := false,
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % scalatestVersion % Test
    ),
    (Compile / compile) := (Compile / compile)
      .dependsOn(RecompileParser.run(`syntax-definition`))
      .value
  )

lazy val `text-buffer` = project
  .in(file("lib/scala/text-buffer"))
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    libraryDependencies ++= Seq(
      "org.typelevel"   %% "cats-core"      % catsVersion,
      "org.bouncycastle" % "bcpkix-jdk15on" % bcpkixJdk15Version,
      "org.scalatest"   %% "scalatest"      % scalatestVersion  % Test,
      "org.scalacheck"  %% "scalacheck"     % scalacheckVersion % Test
    )
  )

lazy val rustParserTargetDirectory =
  SettingKey[File]("target directory for the Rust parser")

(`syntax-rust-definition` / rustParserTargetDirectory) := {
  // setting "debug" for release, because it isn't yet safely integrated into
  // the parser definition
  val versionName = if (BuildInfo.isReleaseMode) "debug" else "debug"
  target.value / "rust" / versionName
}

val generateRustParserLib =
  TaskKey[Seq[File]]("generateRustParserLib", "Generates parser native library")
`syntax-rust-definition` / generateRustParserLib := {
  val log = state.value.log
  val libGlob =
    (`syntax-rust-definition` / rustParserTargetDirectory).value.toGlob / "libenso_parser.so"

  val allLibs = FileTreeView.default.list(Seq(libGlob)).map(_._1)
  if (
    sys.env.get("CI").isDefined ||
    allLibs.isEmpty ||
    (`syntax-rust-definition` / generateRustParserLib).inputFileChanges.hasChanges
  ) {
    val os = System.getProperty("os.name")
    val baseArguments = Seq(
      "build",
      "-p",
      "enso-parser-jni",
      "-Z",
      "unstable-options",
      "--out-dir",
      (`syntax-rust-definition` / rustParserTargetDirectory).value.toString
    )
    val adjustedArguments = baseArguments ++
      (if (BuildInfo.isReleaseMode)
         Seq("--release")
       else Seq())
    Cargo.run(adjustedArguments, log)
  }
  FileTreeView.default.list(Seq(libGlob)).map(_._1.toFile)
}

`syntax-rust-definition` / generateRustParserLib / fileInputs +=
  (`syntax-rust-definition` / baseDirectory).value.toGlob / "jni" / "src" / "*.rs"

val generateParserJavaSources = TaskKey[Seq[File]](
  "generateParserJavaSources",
  "Generates Java sources for Rust parser"
)
`syntax-rust-definition` / generateParserJavaSources := {
  generateRustParser(
    (`syntax-rust-definition` / Compile / sourceManaged).value,
    (`syntax-rust-definition` / generateParserJavaSources).inputFileChanges,
    state.value.log
  )
}
`syntax-rust-definition` / generateParserJavaSources / fileInputs +=
  (`syntax-rust-definition` / baseDirectory).value.toGlob / "generate-java" / "src" / ** / "*.rs"
`syntax-rust-definition` / generateParserJavaSources / fileInputs +=
  (`syntax-rust-definition` / baseDirectory).value.toGlob / "src" / ** / "*.rs"

def generateRustParser(
  base: File,
  changes: sbt.nio.FileChanges,
  log: ManagedLogger
): Seq[File] = {
  import scala.jdk.CollectionConverters._
  import java.nio.file.Paths

  val syntaxPkgs = Paths.get("org", "enso", "syntax2").toString
  val fullPkg    = Paths.get(base.toString, syntaxPkgs).toFile
  if (!fullPkg.exists()) {
    fullPkg.mkdirs()
  }
  if (changes.hasChanges) {
    val args = Seq(
      "run",
      "-p",
      "enso-parser-generate-java",
      "--bin",
      "enso-parser-generate-java",
      fullPkg.toString
    )
    Cargo.run(args, log)
  }
  FileUtils.listFiles(fullPkg, Array("scala", "java"), true).asScala.toSeq
}

lazy val `syntax-rust-definition` = project
  .in(file("lib/rust/parser"))
  .configs(Test)
  .settings(
    Compile / sourceGenerators += generateParserJavaSources,
    Compile / resourceGenerators += generateRustParserLib,
    Compile / javaSource := baseDirectory.value / "generate-java" / "java",
    frgaalJavaCompilerSetting
  )

lazy val pkg = (project in file("lib/scala/pkg"))
  .settings(
    Compile / run / mainClass := Some("org.enso.pkg.Main"),
    frgaalJavaCompilerSetting,
    version := "0.1",
    libraryDependencies ++= circe ++ Seq(
      "org.scalatest"     %% "scalatest"        % scalatestVersion % Test,
      "io.circe"          %% "circe-yaml"       % circeYamlVersion,
      "org.apache.commons" % "commons-compress" % commonsCompressVersion,
      "commons-io"         % "commons-io"       % commonsIoVersion
    )
  )
  .dependsOn(editions)

lazy val `akka-native` = project
  .in(file("lib/scala/akka-native"))
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    version := "0.1",
    libraryDependencies ++= Seq(
      akkaActor
    ),
    // Note [Native Image Workaround for GraalVM 20.2]
    libraryDependencies += "org.graalvm.nativeimage" % "svm" % graalMavenPackagesVersion % "provided"
  )

lazy val `profiling-utils` = project
  .in(file("lib/scala/profiling-utils"))
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    version := "0.1",
    libraryDependencies ++= Seq(
      "org.netbeans.api" % "org-netbeans-modules-sampler" % netbeansApiVersion
      exclude ("org.netbeans.api", "org-openide-loaders")
      exclude ("org.netbeans.api", "org-openide-nodes")
      exclude ("org.netbeans.api", "org-netbeans-api-progress-nb")
      exclude ("org.netbeans.api", "org-netbeans-api-progress")
      exclude ("org.netbeans.api", "org-openide-util-lookup")
      exclude ("org.netbeans.api", "org-openide-util")
      exclude ("org.netbeans.api", "org-openide-dialogs")
      exclude ("org.netbeans.api", "org-openide-filesystems")
      exclude ("org.netbeans.api", "org-openide-util-ui")
      exclude ("org.netbeans.api", "org-openide-awt")
      exclude ("org.netbeans.api", "org-openide-modules")
      exclude ("org.netbeans.api", "org-netbeans-api-annotations-common")
    )
  )

lazy val `logging-utils` = project
  .in(file("lib/scala/logging-utils"))
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    version := "0.1",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % scalatestVersion % Test,
      "org.slf4j"      % "slf4j-api" % slf4jVersion
    ) ++ logbackTest
  )

lazy val `logging-service` = project
  .in(file("lib/scala/logging-service"))
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    version := "0.1",
    libraryDependencies ++= Seq(
      "org.slf4j"      % "slf4j-api" % slf4jVersion,
      "com.typesafe"   % "config"    % typesafeConfigVersion,
      "org.scalatest" %% "scalatest" % scalatestVersion % Test,
      akkaHttp
    )
  )
  .dependsOn(`logging-utils`)
  .dependsOn(`logging-config`)

lazy val `logging-config` = project
  .in(file("lib/scala/logging-config"))
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    version := "0.1",
    libraryDependencies ++= Seq(
      "com.typesafe" % "config"    % typesafeConfigVersion,
      "org.slf4j"    % "slf4j-api" % slf4jVersion
    )
  )

lazy val `logging-service-logback` = project
  .in(file("lib/scala/logging-service-logback"))
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    version := "0.1",
    libraryDependencies ++= Seq(
      "org.slf4j"        % "slf4j-api"               % slf4jVersion,
      "io.sentry"        % "sentry-logback"          % "6.28.0",
      "io.sentry"        % "sentry"                  % "6.28.0",
      "org.netbeans.api" % "org-openide-util-lookup" % netbeansApiVersion % "provided"
    ) ++ logbackPkg
  )
  .dependsOn(`logging-config`)
  .dependsOn(`logging-service`)

lazy val `logging-utils-akka` = project
  .in(file("lib/scala/logging-utils-akka"))
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    version := "0.1",
    libraryDependencies ++= Seq(
      "org.slf4j"          % "slf4j-api"  % slf4jVersion,
      "com.typesafe.akka" %% "akka-actor" % akkaVersion
    )
  )

lazy val filewatcher = project
  .in(file("lib/scala/filewatcher"))
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    version := "0.1",
    libraryDependencies ++= Seq(
      "io.methvin"     % "directory-watcher" % directoryWatcherVersion,
      "commons-io"     % "commons-io"        % commonsIoVersion,
      "org.scalatest" %% "scalatest"         % scalatestVersion % Test
    ),
    Test / fork := true,
    Test / javaOptions ++= testLogProviderOptions
  )
  .dependsOn(testkit % Test)
  .dependsOn(`logging-service-logback` % "test->test")

lazy val `logging-truffle-connector` = project
  .in(file("lib/scala/logging-truffle-connector"))
  .settings(
    frgaalJavaCompilerSetting,
    version := "0.1",
    libraryDependencies ++= Seq(
      "org.slf4j"           % "slf4j-api"               % slf4jVersion,
      "org.graalvm.truffle" % "truffle-api"             % graalMavenPackagesVersion % "provided",
      "org.netbeans.api"    % "org-openide-util-lookup" % netbeansApiVersion        % "provided"
    )
  )
  .dependsOn(`logging-utils`)
  .dependsOn(`polyglot-api`)

lazy val cli = project
  .in(file("lib/scala/cli"))
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    version := "0.1",
    libraryDependencies ++= circe ++ Seq(
      "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion,
      "org.scalatest"              %% "scalatest"     % scalatestVersion % Test,
      "org.typelevel"              %% "cats-core"     % catsVersion
    ),
    Test / parallelExecution := false
  )

lazy val `task-progress-notifications` = project
  .in(file("lib/scala/task-progress-notifications"))
  .configs(Test)
  .settings(
    version := "0.1",
    libraryDependencies ++= Seq(
      "com.beachape"  %% "enumeratum-circe" % enumeratumCirceVersion,
      "org.scalatest" %% "scalatest"        % scalatestVersion % Test
    ),
    Test / parallelExecution := false
  )
  .dependsOn(cli)
  .dependsOn(`json-rpc-server`)

lazy val `version-output` = (project in file("lib/scala/version-output"))
  .settings(
    version := "0.1"
  )
  .settings(
    frgaalJavaCompilerSetting,
    Compile / sourceGenerators += Def.task {
      val file = (Compile / sourceManaged).value / "buildinfo" / "Info.scala"
      BuildInfo
        .writeBuildInfoFile(
          file                  = file,
          log                   = state.value.log,
          defaultDevEnsoVersion = defaultDevEnsoVersion,
          ensoVersion           = ensoVersion,
          scalacVersion         = scalacVersion,
          graalVersion          = graalVersion,
          currentEdition        = currentEdition
        )
    }.taskValue
  )

lazy val `refactoring-utils` = project
  .in(file("lib/scala/refactoring-utils"))
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    commands += WithDebugCommand.withDebug,
    version := "0.1",
    libraryDependencies ++= Seq(
      "junit"          % "junit"           % junitVersion   % Test,
      "com.github.sbt" % "junit-interface" % junitIfVersion % Test
    )
  )
  .dependsOn(`runtime-parser`)
  .dependsOn(`text-buffer`)
  .dependsOn(testkit % Test)

lazy val `project-manager` = (project in file("lib/scala/project-manager"))
  .settings(
    (Compile / mainClass) := Some("org.enso.projectmanager.boot.ProjectManager")
  )
  .settings(
    frgaalJavaCompilerSetting,
    (Compile / run / fork) := true,
    (Test / fork) := true,
    (Compile / run / connectInput) := true,
    commands += WithDebugCommand.withDebug,
    libraryDependencies ++= akka ++ Seq(akkaTestkit % Test),
    libraryDependencies ++= circe,
    libraryDependencies ++= Seq(
      "com.typesafe"                % "config"              % typesafeConfigVersion,
      "com.github.pureconfig"      %% "pureconfig"          % pureconfigVersion,
      "com.typesafe.scala-logging" %% "scala-logging"       % scalaLoggingVersion,
      "dev.zio"                    %% "zio"                 % zioVersion,
      "dev.zio"                    %% "zio-interop-cats"    % zioInteropCatsVersion,
      "commons-cli"                 % "commons-cli"         % commonsCliVersion,
      "commons-io"                  % "commons-io"          % commonsIoVersion,
      "org.apache.commons"          % "commons-lang3"       % commonsLangVersion,
      "com.beachape"               %% "enumeratum-circe"    % enumeratumCirceVersion,
      "com.miguno.akka"            %% "akka-mock-scheduler" % akkaMockSchedulerVersion % Test,
      "org.mockito"                %% "mockito-scala"       % mockitoScalaVersion      % Test
    ),
    addCompilerPlugin(
      "org.typelevel" %% "kind-projector" % kindProjectorVersion cross CrossVersion.full
    )
  )
  .settings(
    assembly / assemblyJarName := "project-manager.jar",
    assembly / test := {},
    assembly / assemblyOutputPath := file("project-manager.jar"),
    assembly / assemblyMergeStrategy := {
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
    (Test / test) := (Test / test).dependsOn(`engine-runner` / assembly).value,
    Test / javaOptions += s"-Dconfig.file=${sourceDirectory.value}/test/resources/application.conf",
    rebuildNativeImage := NativeImage
      .buildNativeImage(
        "project-manager",
        staticOnLinux = true,
        initializeAtRuntime = Seq(
          "scala.util.Random",
          "zio.internal.ZScheduler$$anon$4",
          "zio.Runtime$",
          "zio.FiberRef$"
        )
      )
      .dependsOn(VerifyReflectionSetup.run)
      .dependsOn(assembly)
      .value,
    buildNativeImage := NativeImage
      .incrementalNativeImageBuild(
        rebuildNativeImage,
        "project-manager"
      )
      .value
  )
  .dependsOn(`akka-native`)
  .dependsOn(`version-output`)
  .dependsOn(editions)
  .dependsOn(`edition-updater`)
  .dependsOn(cli)
  .dependsOn(`task-progress-notifications`)
  .dependsOn(`polyglot-api`)
  .dependsOn(`runtime-version-manager`)
  .dependsOn(`library-manager`)
  .dependsOn(`logging-utils-akka`)
  .dependsOn(`logging-service`)
  .dependsOn(pkg)
  .dependsOn(`json-rpc-server`)
  .dependsOn(`logging-service-logback` % Runtime)
  .dependsOn(`json-rpc-server-test` % Test)
  .dependsOn(testkit % Test)
  .dependsOn(`runtime-version-manager-test` % Test)

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
  .in(file("lib/scala/json-rpc-server"))
  .settings(
    frgaalJavaCompilerSetting,
    libraryDependencies ++= akka ++ logbackTest,
    libraryDependencies ++= circe,
    libraryDependencies ++= Seq(
      "io.circe"                   %% "circe-literal"   % circeVersion,
      "com.typesafe.scala-logging" %% "scala-logging"   % scalaLoggingVersion,
      akkaTestkit                   % Test,
      "org.scalatest"              %% "scalatest"       % scalatestVersion      % Test,
      "junit"                       % "junit"           % junitVersion          % Test,
      "com.github.sbt"              % "junit-interface" % junitIfVersion        % Test,
      "org.apache.httpcomponents"   % "httpclient"      % httpComponentsVersion % Test,
      "org.apache.httpcomponents"   % "httpcore"        % httpComponentsVersion % Test,
      "commons-io"                  % "commons-io"      % commonsIoVersion      % Test
    )
  )

lazy val `json-rpc-server-test` = project
  .in(file("lib/scala/json-rpc-server-test"))
  .settings(
    libraryDependencies ++= akka,
    libraryDependencies ++= circe,
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-literal" % circeVersion,
      akkaTestkit,
      "org.scalatest" %% "scalatest"     % scalatestVersion,
      "org.gnieh"     %% "diffson-circe" % diffsonVersion
    )
  )
  .dependsOn(`json-rpc-server`)

lazy val testkit = project
  .in(file("lib/scala/testkit"))
  .settings(
    frgaalJavaCompilerSetting,
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-lang3" % commonsLangVersion,
      "commons-io"         % "commons-io"    % commonsIoVersion,
      "org.scalatest"     %% "scalatest"     % scalatestVersion
    )
  )

lazy val searcher = project
  .in(file("lib/scala/searcher"))
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    libraryDependencies ++= jmh ++ Seq(
      "com.typesafe.slick" %% "slick"       % slickVersion,
      "org.xerial"          % "sqlite-jdbc" % sqliteVersion,
      "org.scalatest"      %% "scalatest"   % scalatestVersion % Test
    ) ++ logbackTest
  )
  .configs(Benchmark)
  .settings(
    inConfig(Benchmark)(Defaults.testSettings),
    Benchmark / fork := true
  )
  .dependsOn(testkit % Test)
  .dependsOn(`polyglot-api`)

lazy val `interpreter-dsl` = (project in file("lib/scala/interpreter-dsl"))
  .settings(
    version := "0.1",
    frgaalJavaCompilerSetting,
    Compile / javacOptions := ((Compile / javacOptions).value ++
    // Only run ServiceProvider processor and ignore those defined in META-INF, thus
    // fixing incremental compilation setup
    Seq(
      "-processor",
      "org.netbeans.modules.openide.util.ServiceProviderProcessor"
    )),
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-lang3"           % commonsLangVersion,
      "org.netbeans.api"   % "org-openide-util-lookup" % netbeansApiVersion,
      "com.google.guava"   % "guava"                   % guavaVersion exclude ("com.google.code.findbugs", "jsr305")
    )
  )

lazy val `interpreter-dsl-test` =
  (project in file("engine/interpreter-dsl-test"))
    .configs(Test)
    .settings(
      version := "0.1",
      frgaalJavaCompilerSetting,
      Test / fork := true,
      Test / javaOptions ++= Seq(
        "-Dgraalvm.locatorDisabled=true",
        s"--upgrade-module-path=${file("engine/runtime/build-cache/truffle-api.jar").absolutePath}"
      ),
      Test / javacOptions ++= Seq(
        "-s",
        (Test / sourceManaged).value.getAbsolutePath
      ),
      Compile / logManager :=
        sbt.internal.util.CustomLogManager.excludeMsg(
          "Could not determine source for class ",
          Level.Warn
        ),
      commands += WithDebugCommand.withDebug,
      libraryDependencies ++= Seq(
        "org.graalvm.truffle" % "truffle-api"           % graalMavenPackagesVersion % "provided",
        "org.graalvm.truffle" % "truffle-dsl-processor" % graalMavenPackagesVersion % "provided",
        "junit"               % "junit"                 % junitVersion              % Test,
        "com.github.sbt"      % "junit-interface"       % junitIfVersion            % Test
      )
    )
    .dependsOn(`interpreter-dsl`)
    .dependsOn(`runtime`)

// ============================================================================
// === Sub-Projects ===========================================================
// ============================================================================

val truffleRunOptionsNoAssert =
  if (java.lang.Boolean.getBoolean("bench.compileOnly")) {
    Seq(
      "-Dpolyglot.engine.IterativePartialEscape=true",
      "-Dpolyglot.engine.BackgroundCompilation=false",
      "-Dbench.compileOnly=true"
    )
  } else {
    Seq(
      "-Dpolyglot.engine.IterativePartialEscape=true",
      "-Dpolyglot.engine.BackgroundCompilation=false"
    )
  }
val truffleRunOptions = "-ea" +: truffleRunOptionsNoAssert

val truffleRunOptionsNoAssertSettings = Seq(
  fork := true,
  javaOptions ++= truffleRunOptionsNoAssert
)

val truffleRunOptionsSettings = Seq(
  fork := true,
  javaOptions ++= truffleRunOptions
)

val testLogProviderOptions = Seq(
  "-Dslf4j.provider=org.enso.logger.TestLogProvider"
)

lazy val `polyglot-api` = project
  .in(file("engine/polyglot-api"))
  .settings(
    frgaalJavaCompilerSetting,
    Test / fork := true,
    commands += WithDebugCommand.withDebug,
    Test / envVars ++= distributionEnvironmentOverrides,
    Test / javaOptions ++= {
      // Note [Classpath Separation]
      val runtimeClasspath =
        (LocalProject("runtime") / Compile / fullClasspath).value
          .map(_.data)
          .mkString(File.pathSeparator)
      Seq(s"-Dtruffle.class.path.append=$runtimeClasspath")
    },
    libraryDependencies ++= Seq(
      "org.graalvm.sdk"        % "polyglot-tck"     % graalMavenPackagesVersion % "provided",
      "org.graalvm.truffle"    % "truffle-api"      % graalMavenPackagesVersion % "provided",
      "com.google.flatbuffers" % "flatbuffers-java" % flatbuffersVersion,
      "org.scalatest"         %% "scalatest"        % scalatestVersion          % Test,
      "org.scalacheck"        %% "scalacheck"       % scalacheckVersion         % Test
    ),
    libraryDependencies ++= jackson,
    GenerateFlatbuffers.flatcVersion := flatbuffersVersion,
    Compile / sourceGenerators += GenerateFlatbuffers.task
  )
  .dependsOn(pkg)
  .dependsOn(`text-buffer`)
  .dependsOn(`logging-utils`)
  .dependsOn(testkit % Test)

lazy val `language-server` = (project in file("engine/language-server"))
  .settings(
    commands += WithDebugCommand.withDebug,
    frgaalJavaCompilerSetting,
    libraryDependencies ++= akka ++ circe ++ Seq(
      "org.slf4j"                   % "slf4j-api"            % slf4jVersion,
      "com.typesafe.scala-logging" %% "scala-logging"        % scalaLoggingVersion,
      "io.circe"                   %% "circe-generic-extras" % circeGenericExtrasVersion,
      "io.circe"                   %% "circe-literal"        % circeVersion,
      "dev.zio"                    %% "zio"                  % zioVersion,
      "com.beachape"               %% "enumeratum-circe"     % enumeratumCirceVersion,
      "com.google.flatbuffers"      % "flatbuffers-java"     % flatbuffersVersion,
      "commons-io"                  % "commons-io"           % commonsIoVersion,
      akkaTestkit                   % Test,
      "com.typesafe.akka"          %% "akka-http-testkit"    % akkaHTTPVersion           % Test,
      "org.scalatest"              %% "scalatest"            % scalatestVersion          % Test,
      "org.scalacheck"             %% "scalacheck"           % scalacheckVersion         % Test,
      "org.graalvm.sdk"             % "polyglot-tck"         % graalMavenPackagesVersion % "provided",
      "org.eclipse.jgit"            % "org.eclipse.jgit"     % jgitVersion
    ),
    Test / testOptions += Tests
      .Argument(TestFrameworks.ScalaCheck, "-minSuccessfulTests", "1000"),
    Test / envVars ++= distributionEnvironmentOverrides,
    GenerateFlatbuffers.flatcVersion := flatbuffersVersion,
    Compile / sourceGenerators += GenerateFlatbuffers.task
  )
  .configs(Benchmark)
  .settings(
    inConfig(Compile)(truffleRunOptionsSettings),
    inConfig(Benchmark)(Defaults.testSettings),
    bench := (Benchmark / test).value,
    libraryDependencies += "com.storm-enroute" %% "scalameter" % scalameterVersion % "bench",
    testFrameworks ++= List(
      new TestFramework("org.scalameter.ScalaMeterFramework")
    )
  )
  .settings(
    // These settings are needed by language-server tests that create a runtime context.
    Test / fork := true,
    Test / javaOptions ++= {
      // Note [Classpath Separation]
      val runtimeClasspath =
        (LocalProject("runtime") / Runtime / fullClasspath).value
          .map(_.data)
          .mkString(File.pathSeparator)
      Seq(
        s"-Dconfig.file=${sourceDirectory.value}/test/resources/application.conf",
        s"-Dtruffle.class.path.append=$runtimeClasspath",
        s"-Duser.dir=${file(".").getCanonicalPath}"
      )
    },
    Test / compile := (Test / compile)
      .dependsOn(LocalProject("enso") / updateLibraryManifests)
      .value,
    Test / envVars ++= Map(
      "ENSO_EDITION_PATH" -> file("distribution/editions").getCanonicalPath
    )
  )
  .dependsOn(`json-rpc-server-test` % Test)
  .dependsOn(`json-rpc-server`)
  .dependsOn(`task-progress-notifications`)
  .dependsOn(`library-manager`)
  .dependsOn(`connected-lock-manager`)
  .dependsOn(`edition-updater`)
  .dependsOn(`logging-utils-akka`)
  .dependsOn(`logging-service`)
  .dependsOn(`polyglot-api`)
  .dependsOn(`searcher`)
  .dependsOn(`text-buffer`)
  .dependsOn(`version-output`)
  .dependsOn(pkg)
  .dependsOn(`profiling-utils`)
  .dependsOn(filewatcher)
  .dependsOn(testkit % Test)
  .dependsOn(`logging-service-logback` % "test->test")
  .dependsOn(`library-manager-test` % Test)
  .dependsOn(`runtime-version-manager-test` % Test)

lazy val cleanInstruments = taskKey[Unit](
  "Cleans fragile class files to force a full recompilation and preserve" +
  "consistency of instrumentation configuration."
)

/** Overrides for the environment variables related to the distribution, so that
  * a local installation does not interfere with runtime tests.
  */
val distributionEnvironmentOverrides = {
  val fakeDir = file("target/fake_dir").getAbsolutePath
  Map(
    "ENSO_DATA_DIRECTORY"           -> fakeDir,
    "ENSO_CONFIG_DIRECTORY"         -> fakeDir,
    "ENSO_RUNTIME_DIRECTORY"        -> file("target/run").getAbsolutePath,
    "ENSO_LOG_DIRECTORY"            -> file("target/logs").getAbsolutePath,
    "ENSO_HOME"                     -> fakeDir,
    "ENSO_EDITION_PATH"             -> "",
    "ENSO_LIBRARY_PATH"             -> "",
    "ENSO_AUXILIARY_LIBRARY_CACHES" -> ""
  )
}

val frgaalSourceLevel = FrgaalJavaCompiler.sourceLevel

lazy val truffleDslSuppressWarnsSetting = Seq(
  Compile / javacOptions ++= Seq(
    "-Atruffle.dsl.SuppressWarnings=truffle-inlining"
  )
)

/** A setting to replace javac with Frgaal compiler, allowing to use latest Java features in the code
  * and still compile down to JDK 11
  */
lazy val frgaalJavaCompilerSetting = Seq(
  Compile / compile / compilers := FrgaalJavaCompiler.compilers(
    (Compile / dependencyClasspath).value,
    compilers.value,
    targetJavaVersion
  ),
  // This dependency is needed only so that developers don't download Frgaal manually.
  // Sadly it cannot be placed under plugins either because meta dependencies are not easily
  // accessible from the non-meta build definition.
  libraryDependencies += FrgaalJavaCompiler.frgaal,
  // Ensure that our tooling uses the right Java version for checking the code.
  Compile / javacOptions ++= Seq(
    "-source",
    frgaalSourceLevel,
    "--enable-preview"
  )
)

lazy val instrumentationSettings = frgaalJavaCompilerSetting ++ Seq(
  version := ensoVersion,
  commands += WithDebugCommand.withDebug,
  Compile / logManager :=
    sbt.internal.util.CustomLogManager
      .excludeMsg("Could not determine source for class ", Level.Warn),
  Compile / javacOptions --= Seq(
    "-source",
    frgaalSourceLevel,
    "--enable-preview"
  ),
  libraryDependencies ++= Seq(
    "org.graalvm.truffle" % "truffle-api"           % graalMavenPackagesVersion % "provided",
    "org.graalvm.truffle" % "truffle-dsl-processor" % graalMavenPackagesVersion % "provided"
  ),
  (Compile / javacOptions) ++= Seq(
    "-s",
    (Compile / sourceManaged).value.getAbsolutePath,
    "-Xlint:unchecked"
  )
)

lazy val `runtime-language-epb` =
  (project in file("engine/runtime-language-epb"))
    .settings(
      inConfig(Compile)(truffleRunOptionsSettings),
      truffleDslSuppressWarnsSetting,
      instrumentationSettings
    )
    .dependsOn(`polyglot-api`)

/** Runs gu (GraalVM updater) command with given `args`.
  * For example `runGu(Seq("install", "js"))`.
  * @param logger Logger for the `gu` command.
  * @param args Arguments for the `gu` command.
  */
def runGu(logger: ManagedLogger, args: Seq[String]): String = {
  val javaHome = new File(
    System.getProperty("java.home")
  )
  val os = {
    if (Platform.isLinux) DistributionPackage.OS.Linux
    else if (Platform.isMacOS) DistributionPackage.OS.MacOS
    else if (Platform.isWindows) DistributionPackage.OS.Windows
    else throw new RuntimeException("Unknown platform")
  }
  packageBuilder.gu(
    logger,
    os,
    javaHome,
    args: _*
  )
}

lazy val runtime = (project in file("engine/runtime"))
  .configs(Benchmark)
  .settings(
    frgaalJavaCompilerSetting,
    truffleDslSuppressWarnsSetting,
    Compile / logManager :=
      sbt.internal.util.CustomLogManager.excludeMsg(
        "Could not determine source for class ",
        Level.Warn
      ),
    version := ensoVersion,
    commands += WithDebugCommand.withDebug,
    inConfig(Compile)(truffleRunOptionsSettings),
    inConfig(Benchmark)(Defaults.testSettings),
    Benchmark / javacOptions --= Seq(
      "-source",
      frgaalSourceLevel,
      "--enable-preview"
    ),
    Test / parallelExecution := false,
    Test / logBuffered := false,
    Test / testOptions += Tests.Argument(
      "-oD"
    ), // show timings for individual tests
    scalacOptions += "-Ymacro-annotations",
    scalacOptions ++= Seq("-Ypatmat-exhaust-depth", "off"),
    libraryDependencies ++= jmh ++ jaxb ++ circe ++ Seq(
      "org.apache.commons"  % "commons-lang3"         % commonsLangVersion,
      "org.apache.tika"     % "tika-core"             % tikaVersion,
      "org.graalvm.sdk"     % "graal-sdk"             % graalMavenPackagesVersion % "provided",
      "org.graalvm.sdk"     % "polyglot-tck"          % graalMavenPackagesVersion % "provided",
      "org.graalvm.truffle" % "truffle-api"           % graalMavenPackagesVersion % "provided",
      "org.graalvm.truffle" % "truffle-dsl-processor" % graalMavenPackagesVersion % "provided",
      "org.graalvm.truffle" % "truffle-tck"           % graalMavenPackagesVersion % "provided",
      "org.graalvm.truffle" % "truffle-tck-common"    % graalMavenPackagesVersion % "provided",
      "org.scalacheck"     %% "scalacheck"            % scalacheckVersion         % Test,
      "org.scalactic"      %% "scalactic"             % scalacticVersion          % Test,
      "org.scalatest"      %% "scalatest"             % scalatestVersion          % Test,
      "org.graalvm.truffle" % "truffle-api"           % graalMavenPackagesVersion % Benchmark,
      "org.typelevel"      %% "cats-core"             % catsVersion,
      "junit"               % "junit"                 % junitVersion              % Test,
      "com.github.sbt"      % "junit-interface"       % junitIfVersion            % Test,
      "org.hamcrest"        % "hamcrest-all"          % hamcrestVersion           % Test,
      "com.lihaoyi"        %% "fansi"                 % fansiVersion
    ),
    Compile / compile / compileInputs := (Compile / compile / compileInputs)
      .dependsOn(CopyTruffleJAR.preCompileTask)
      .value,
    // Note [Classpath Separation]
    Test / javaOptions ++= testLogProviderOptions ++ Seq(
      "-Dgraalvm.locatorDisabled=true",
      s"--upgrade-module-path=${file("engine/runtime/build-cache/truffle-api.jar").absolutePath}"
    ),
    Test / fork := true,
    Test / envVars ++= distributionEnvironmentOverrides ++ Map(
      "ENSO_TEST_DISABLE_IR_CACHE" -> "false"
    ),
    Global / onLoad := GraalVersionCheck.addVersionCheck(
      graalVersion
    )((Global / onLoad).value),
    bootstrap := CopyTruffleJAR.bootstrapJARs.value
  )
  .settings(
    (Compile / javacOptions) ++= Seq(
      "-s",
      (Compile / sourceManaged).value.getAbsolutePath,
      "-Xlint:unchecked"
    )
  )
  .settings(
    (Compile / compile) := (Compile / compile)
      .dependsOn(Def.task { (Compile / sourceManaged).value.mkdirs })
      .value
  )
  .settings(
    (Runtime / compile) := (Runtime / compile)
      .dependsOn(`std-base` / Compile / packageBin)
      .dependsOn(`enso-test-java-helpers` / Compile / packageBin)
      .dependsOn(`benchmark-java-helpers` / Compile / packageBin)
      .dependsOn(`exploratory-benchmark-java-helpers` / Compile / packageBin)
      .dependsOn(`std-image` / Compile / packageBin)
      .dependsOn(`std-database` / Compile / packageBin)
      .dependsOn(`std-google-api` / Compile / packageBin)
      .dependsOn(`std-table` / Compile / packageBin)
      .dependsOn(`std-aws` / Compile / packageBin)
      .value
  )
  .settings(
    bench := (Benchmark / test).tag(Exclusive).value,
    benchOnly := Def.inputTaskDyn {
      import complete.Parsers.spaceDelimited
      val name = spaceDelimited("<name>").parsed match {
        case List(name) => name
        case _          => throw new IllegalArgumentException("Expected one argument.")
      }
      Def.task {
        (Benchmark / testOnly).toTask(" -- -z " + name).value
      }
    }.evaluated,
    Benchmark / parallelExecution := false
  )
  .dependsOn(`common-polyglot-core-utils`)
  .dependsOn(`edition-updater`)
  .dependsOn(`interpreter-dsl`)
  .dependsOn(`library-manager`)
  .dependsOn(`logging-truffle-connector`)
  .dependsOn(`polyglot-api`)
  .dependsOn(`text-buffer`)
  .dependsOn(`runtime-parser`)
  .dependsOn(pkg)
  .dependsOn(`connected-lock-manager`)
  .dependsOn(testkit % Test)
  .dependsOn(`logging-service-logback` % "test->test")

lazy val `runtime-parser` =
  (project in file("engine/runtime-parser"))
    .settings(
      frgaalJavaCompilerSetting,
      instrumentationSettings,
      libraryDependencies ++= Seq(
        "junit"          % "junit"           % junitVersion     % Test,
        "com.github.sbt" % "junit-interface" % junitIfVersion   % Test,
        "org.scalatest" %% "scalatest"       % scalatestVersion % Test
      )
    )
    .dependsOn(syntax)
    .dependsOn(`syntax-rust-definition`)

lazy val `runtime-instrument-common` =
  (project in file("engine/runtime-instrument-common"))
    .configs(Benchmark)
    .settings(
      frgaalJavaCompilerSetting,
      inConfig(Compile)(truffleRunOptionsSettings),
      inConfig(Benchmark)(Defaults.testSettings),
      instrumentationSettings,
      Test / javaOptions ++= Seq(
        "-Dgraalvm.locatorDisabled=true",
        s"--upgrade-module-path=${file("engine/runtime/build-cache/truffle-api.jar").absolutePath}"
      ),
      bench := (Benchmark / test).tag(Exclusive).value,
      Benchmark / parallelExecution := false,
      Test / fork := true,
      Test / envVars ++= distributionEnvironmentOverrides ++ Map(
        "ENSO_TEST_DISABLE_IR_CACHE" -> "false"
      )
    )
    .dependsOn(`refactoring-utils`)
    .dependsOn(
      runtime % "compile->compile;test->test;runtime->runtime;bench->bench"
    )

lazy val `runtime-instrument-id-execution` =
  (project in file("engine/runtime-instrument-id-execution"))
    .settings(
      frgaalJavaCompilerSetting,
      inConfig(Compile)(truffleRunOptionsSettings),
      instrumentationSettings
    )
    .dependsOn(runtime)
    .dependsOn(`runtime-instrument-common`)

lazy val `runtime-instrument-repl-debugger` =
  (project in file("engine/runtime-instrument-repl-debugger"))
    .settings(
      inConfig(Compile)(truffleRunOptionsSettings),
      instrumentationSettings
    )
    .dependsOn(runtime)
    .dependsOn(`runtime-instrument-common`)

lazy val `runtime-instrument-runtime-server` =
  (project in file("engine/runtime-instrument-runtime-server"))
    .settings(
      inConfig(Compile)(truffleRunOptionsSettings),
      instrumentationSettings
    )
    .dependsOn(runtime)
    .dependsOn(`runtime-instrument-common`)

lazy val `runtime-with-instruments` =
  (project in file("engine/runtime-with-instruments"))
    .configs(Benchmark)
    .settings(
      frgaalJavaCompilerSetting,
      inConfig(Compile)(truffleRunOptionsSettings),
      inConfig(Benchmark)(Defaults.testSettings),
      commands += WithDebugCommand.withDebug,
      Benchmark / javacOptions --= Seq(
        "-source",
        frgaalSourceLevel,
        "--enable-preview"
      ),
      Test / javaOptions ++= testLogProviderOptions ++ Seq(
        "-Dgraalvm.locatorDisabled=true",
        s"--upgrade-module-path=${file("engine/runtime/build-cache/truffle-api.jar").absolutePath}"
      ),
      Test / fork := true,
      Test / envVars ++= distributionEnvironmentOverrides ++ Map(
        "ENSO_TEST_DISABLE_IR_CACHE" -> "false"
      ),
      libraryDependencies ++= Seq(
        "org.scalatest"      %% "scalatest"             % scalatestVersion          % Test,
        "org.graalvm.truffle" % "truffle-api"           % graalMavenPackagesVersion % Test,
        "org.graalvm.truffle" % "truffle-dsl-processor" % graalMavenPackagesVersion % Test
      ),
      // Note [Unmanaged Classpath]
      Test / unmanagedClasspath += (baseDirectory.value / ".." / ".." / "app" / "gui" / "view" / "graph-editor" / "src" / "builtin" / "visualization" / "native" / "inc"),
      assembly / assemblyJarName := "runtime.jar",
      assembly / test := {},
      assembly / assemblyOutputPath := file("runtime.jar"),
      assembly / assemblyMergeStrategy := {
        case PathList("META-INF", file, xs @ _*) if file.endsWith(".DSA") =>
          MergeStrategy.discard
        case PathList("META-INF", file, xs @ _*) if file.endsWith(".SF") =>
          MergeStrategy.discard
        case PathList("META-INF", "MANIFEST.MF", xs @ _*) =>
          MergeStrategy.discard
        case PathList("META-INF", "services", xs @ _*) =>
          MergeStrategy.concat
        case _ => MergeStrategy.first
      }
    )
    .dependsOn(runtime % "compile->compile;test->test;runtime->runtime")
    .dependsOn(`runtime-instrument-common`)
    .dependsOn(`runtime-instrument-id-execution`)
    .dependsOn(`runtime-instrument-repl-debugger`)
    .dependsOn(`runtime-instrument-runtime-server`)
    .dependsOn(`runtime-language-epb`)
    .dependsOn(`logging-service-logback` % "test->test")

/* runtime-with-polyglot
 * ~~~~~~~~~~~~~~~~~~~~~
 * A project that allows loading polyglot languages by not disabling
 * the GraalVM locator explicitly with `-Dgraalvm.locatorDisabled=true` like the
 * `runtime-with-instruments` project does. It means that the test classes and
 * runtime classes are loaded using different classloaders, and the test code
 * cannot access the `EnsoContext`.
 */

lazy val `runtime-with-polyglot` =
  (project in file("engine/runtime-with-polyglot"))
    .configs(Benchmark)
    .settings(
      frgaalJavaCompilerSetting,
      inConfig(Compile)(truffleRunOptionsNoAssertSettings),
      inConfig(Benchmark)(Defaults.testSettings),
      commands += WithDebugCommand.withDebug,
      Benchmark / javacOptions --= Seq(
        "-source",
        frgaalSourceLevel,
        "--enable-preview"
      ),
      Test / javaOptions ++= {
        // Note [Classpath Separation]
        val runtimeClasspath =
          (LocalProject("runtime") / Compile / fullClasspath).value
        val runtimeInstrumentsClasspath =
          (LocalProject(
            "runtime-with-instruments"
          ) / Compile / fullClasspath).value
        val appendClasspath =
          (runtimeClasspath ++ runtimeInstrumentsClasspath)
            .map(_.data)
            .mkString(File.pathSeparator)
        Seq(
          s"-Dtruffle.class.path.append=$appendClasspath"
        )
      },
      Test / fork := true,
      Test / envVars ++= distributionEnvironmentOverrides ++ Map(
        "ENSO_TEST_DISABLE_IR_CACHE" -> "false"
      ),
      libraryDependencies ++= Seq(
        "org.graalvm.sdk" % "graal-sdk" % graalMavenPackagesVersion % "provided",
        "org.scalatest"  %% "scalatest" % scalatestVersion          % Test
      )
    )
    .dependsOn(runtime % "compile->compile;test->test;runtime->runtime")
    .dependsOn(`runtime-with-instruments`)

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

lazy val `engine-runner` = project
  .in(file("engine/runner"))
  .settings(
    frgaalJavaCompilerSetting,
    javaOptions ++= {
      // Note [Classpath Separation]
      val runtimeClasspath =
        (runtime / Compile / fullClasspath).value
          .map(_.data)
          .mkString(File.pathSeparator)
      Seq(s"-Dtruffle.class.path.append=$runtimeClasspath")
    },
    packageOptions := Seq(
      // The `Multi-Release: true` comes from the `org.xerial/sqlite-jdbc` dependency.
      // But the current version of sbt-assembly does not allow to merge MANIFEST.MF
      // files this way.
      Package.ManifestAttributes(("Multi-Release", "true"))
    ),
    Compile / run / mainClass := Some("org.enso.runner.Main"),
    assembly / mainClass := (Compile / run / mainClass).value,
    assembly / assemblyJarName := "runner.jar",
    assembly / test := {},
    assembly / assemblyOutputPath := file("runner.jar"),
    assembly / assemblyMergeStrategy := {
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
    commands += WithDebugCommand.withDebug,
    inConfig(Compile)(truffleRunOptionsSettings),
    libraryDependencies ++= Seq(
      "org.graalvm.sdk"     % "polyglot-tck" % graalMavenPackagesVersion % Provided,
      "org.graalvm.truffle" % "truffle-api"  % graalMavenPackagesVersion % Provided,
      "commons-cli"         % "commons-cli"  % commonsCliVersion,
      "com.monovore"       %% "decline"      % declineVersion,
      "org.jline"           % "jline"        % jlineVersion,
      "org.typelevel"      %% "cats-core"    % catsVersion
    ),
    run / connectInput := true
  )
  .settings(
    assembly := assembly
      .dependsOn(`runtime-with-instruments` / assembly)
      .value,
    rebuildNativeImage := NativeImage
      .buildNativeImage(
        "runner",
        staticOnLinux = false,
        additionalOptions = Seq(
          "-Dorg.apache.commons.logging.Log=org.apache.commons.logging.impl.NoOpLog",
          "-H:IncludeResources=.*Main.enso$",
          "--macro:truffle",
          "--language:js",
          // "-g",
          //          "-H:+DashboardAll",
          //          "-H:DashboardDump=runner.bgv"
          "-Dnic=nic"
        ) ++ (if (
                org.graalvm.polyglot.Engine
                  .create()
                  .getLanguages()
                  .containsKey("java")
              ) {
                Seq(
                  "-Dorg.graalvm.launcher.home=" + System.getProperty(
                    "java.home"
                  ),
                  "--language:java"
                )
              } else {
                Seq()
              }),
        mainClass = Option("org.enso.runner.Main"),
        cp        = Option("runtime.jar"),
        initializeAtRuntime = Seq(
          "org.jline.nativ.JLineLibrary",
          "io.methvin.watchservice.jna.CarbonAPI",
          "org.enso.syntax2.Parser",
          "zio.internal.ZScheduler$$anon$4",
          "org.enso.runner.Main$",
          "sun.awt",
          "sun.java2d",
          "sun.font",
          "java.awt",
          "com.sun.imageio",
          "akka.http"
        )
      )
      .dependsOn(assembly)
      .value,
    buildNativeImage := NativeImage
      .incrementalNativeImageBuild(
        rebuildNativeImage,
        "runner"
      )
      .value
  )
  .dependsOn(`version-output`)
  .dependsOn(pkg)
  .dependsOn(cli)
  .dependsOn(`library-manager`)
  .dependsOn(`language-server`)
  .dependsOn(`edition-updater`)
  .dependsOn(`logging-service`)
  .dependsOn(`logging-service-logback` % Runtime)
  .dependsOn(`polyglot-api`)

lazy val launcher = project
  .in(file("engine/launcher"))
  .configs(Test)
  .settings(
    resolvers += Resolver.bintrayRepo("gn0s1s", "releases"),
    libraryDependencies ++= Seq(
      "com.typesafe.scala-logging" %% "scala-logging"    % scalaLoggingVersion,
      "org.typelevel"              %% "cats-core"        % catsVersion,
      "nl.gn0s1s"                  %% "bump"             % bumpVersion,
      "org.apache.commons"          % "commons-compress" % commonsCompressVersion,
      "org.scalatest"              %% "scalatest"        % scalatestVersion % Test,
      akkaSLF4J
    )
  )
  .settings(
    rebuildNativeImage := NativeImage
      .buildNativeImage(
        "enso",
        staticOnLinux = true,
        additionalOptions = Seq(
          "-Dorg.apache.commons.logging.Log=org.apache.commons.logging.impl.NoOpLog",
          "-H:IncludeResources=.*Main.enso$"
        )
      )
      .dependsOn(assembly)
      .dependsOn(VerifyReflectionSetup.run)
      .value,
    buildNativeImage := NativeImage
      .incrementalNativeImageBuild(
        rebuildNativeImage,
        "enso"
      )
      .value,
    assembly / test := {},
    assembly / assemblyOutputPath := file("launcher.jar"),
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", file, xs @ _*) if file.endsWith(".DSA") =>
        MergeStrategy.discard
      case PathList("META-INF", file, xs @ _*) if file.endsWith(".SF") =>
        MergeStrategy.discard
      case PathList("META-INF", "MANIFEST.MF", xs @ _*) =>
        MergeStrategy.discard
      case "application.conf" => MergeStrategy.concat
      case "reference.conf"   => MergeStrategy.concat
      case x =>
        MergeStrategy.first
    }
  )
  .settings(
    Test / fork := true,
    Test / javaOptions ++= testLogProviderOptions,
    (Test / test) := (Test / test)
      .dependsOn(buildNativeImage)
      .dependsOn(LauncherShimsForTest.prepare())
      .value,
    (Test / testOnly) := (Test / testOnly)
      .dependsOn(buildNativeImage)
      .dependsOn(LauncherShimsForTest.prepare())
      .evaluated,
    Test / fork := true,
    Test / javaOptions += s"-Dconfig.file=${sourceDirectory.value}/test/resources/application.conf"
  )
  .dependsOn(cli)
  .dependsOn(`runtime-version-manager`)
  .dependsOn(`version-output`)
  .dependsOn(pkg)
  .dependsOn(`logging-utils` % "test->test")
  .dependsOn(`logging-service`)
  .dependsOn(`logging-service-logback` % "test->test;runtime->runtime")
  .dependsOn(`distribution-manager` % Test)
  .dependsOn(`runtime-version-manager-test` % Test)

lazy val `distribution-manager` = project
  .in(file("lib/scala/distribution-manager"))
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    resolvers += Resolver.bintrayRepo("gn0s1s", "releases"),
    libraryDependencies ++= Seq(
      "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion,
      "io.circe"                   %% "circe-yaml"    % circeYamlVersion,
      "commons-io"                  % "commons-io"    % commonsIoVersion,
      "org.scalatest"              %% "scalatest"     % scalatestVersion % Test
    )
  )
  .dependsOn(editions)
  .dependsOn(cli)
  .dependsOn(pkg)
  .dependsOn(`logging-utils`)

lazy val `bench-processor` = (project in file("lib/scala/bench-processor"))
  .settings(
    frgaalJavaCompilerSetting,
    libraryDependencies ++= Seq(
      "jakarta.xml.bind"    % "jakarta.xml.bind-api"     % jaxbVersion,
      "com.sun.xml.bind"    % "jaxb-impl"                % jaxbVersion,
      "org.openjdk.jmh"     % "jmh-core"                 % jmhVersion                % "provided",
      "org.openjdk.jmh"     % "jmh-generator-annprocess" % jmhVersion                % "provided",
      "org.netbeans.api"    % "org-openide-util-lookup"  % netbeansApiVersion        % "provided",
      "org.graalvm.sdk"     % "graal-sdk"                % graalMavenPackagesVersion % "provided",
      "junit"               % "junit"                    % junitVersion              % Test,
      "com.github.sbt"      % "junit-interface"          % junitIfVersion            % Test,
      "org.graalvm.truffle" % "truffle-api"              % graalMavenPackagesVersion % Test
    ),
    Compile / javacOptions := ((Compile / javacOptions).value ++
    // Only run ServiceProvider processor and ignore those defined in META-INF, thus
    // fixing incremental compilation setup
    Seq(
      "-processor",
      "org.netbeans.modules.openide.util.ServiceProviderProcessor"
    )),
    commands += WithDebugCommand.withDebug,
    (Test / fork) := true,
    (Test / parallelExecution) := false,
    (Test / javaOptions) ++= {
      val runtimeJars =
        (LocalProject("runtime") / Compile / fullClasspath).value
      val jarsStr = runtimeJars.map(_.data).mkString(File.pathSeparator)
      Seq(
        s"-Dtruffle.class.path.append=${jarsStr}"
      )
    }
  )
  .dependsOn(`polyglot-api`)
  .dependsOn(runtime)

lazy val `std-benchmarks` = (project in file("std-bits/benchmarks"))
  .settings(
    frgaalJavaCompilerSetting,
    libraryDependencies ++= jmh ++ Seq(
      "org.openjdk.jmh" % "jmh-core"                 % jmhVersion                % Benchmark,
      "org.openjdk.jmh" % "jmh-generator-annprocess" % jmhVersion                % Benchmark,
      "org.graalvm.sdk" % "graal-sdk"                % graalMavenPackagesVersion % Benchmark
    ),
    commands += WithDebugCommand.withDebug,
    (Compile / logManager) :=
      sbt.internal.util.CustomLogManager.excludeMsg(
        "Could not determine source for class ",
        Level.Warn
      )
  )
  .configs(Benchmark)
  .settings(
    inConfig(Benchmark)(Defaults.testSettings)
  )
  .settings(
    (Benchmark / parallelExecution) := false,
    (Benchmark / run / fork) := true,
    (Benchmark / run / connectInput) := true,
    // Pass -Dtruffle.class.path.append to javac
    (Benchmark / compile / javacOptions) ++= {
      val runtimeClasspath =
        (LocalProject("runtime") / Compile / fullClasspath).value
      val runtimeInstrumentsClasspath =
        (LocalProject(
          "runtime-with-instruments"
        ) / Compile / fullClasspath).value
      val appendClasspath =
        (runtimeClasspath ++ runtimeInstrumentsClasspath)
          .map(_.data)
          .mkString(File.pathSeparator)
      Seq(
        s"-J-Dtruffle.class.path.append=$appendClasspath"
      )
    },
    (Benchmark / compile / javacOptions) ++= Seq(
      "-s",
      (Benchmark / sourceManaged).value.getAbsolutePath,
      "-Xlint:unchecked"
    ),
    (Benchmark / run / javaOptions) ++= {
      val runtimeClasspath =
        (LocalProject("runtime") / Compile / fullClasspath).value
      val runtimeInstrumentsClasspath =
        (LocalProject(
          "runtime-with-instruments"
        ) / Compile / fullClasspath).value
      val appendClasspath =
        (runtimeClasspath ++ runtimeInstrumentsClasspath)
          .map(_.data)
          .mkString(File.pathSeparator)
      Seq(
        s"-Dtruffle.class.path.append=$appendClasspath"
      )
    }
  )
  .settings(
    bench := Def
      .task {
        (Benchmark / run).toTask("").tag(Exclusive).value
      }
      .dependsOn(
        buildEngineDistribution
      )
      .value,
    benchOnly := Def.inputTaskDyn {
      import complete.Parsers.spaceDelimited
      val name = spaceDelimited("<name>").parsed match {
        case List(name) => name
        case _          => throw new IllegalArgumentException("Expected one argument.")
      }
      Def.task {
        (Benchmark / run).toTask(" " + name).value
      }
    }.evaluated
  )
  .dependsOn(`bench-processor` % Benchmark)
  .dependsOn(runtime % Benchmark)

lazy val editions = project
  .in(file("lib/scala/editions"))
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    resolvers += Resolver.bintrayRepo("gn0s1s", "releases"),
    libraryDependencies ++= Seq(
      "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion,
      "nl.gn0s1s"                  %% "bump"          % bumpVersion,
      "io.circe"                   %% "circe-yaml"    % circeYamlVersion,
      "org.scalatest"              %% "scalatest"     % scalatestVersion % Test
    )
  )
  .settings(
    (Compile / compile) := (Compile / compile)
      .dependsOn(
        Def.task {
          Editions.writeEditionConfig(
            editionsRoot   = file("distribution") / "editions",
            ensoVersion    = ensoVersion,
            editionName    = currentEdition,
            libraryVersion = stdLibVersion,
            log            = streams.value.log
          )
        }
      )
      .value,
    cleanFiles += baseDirectory.value / ".." / ".." / "distribution" / "editions"
  )
  .dependsOn(testkit % Test)

lazy val downloader = (project in file("lib/scala/downloader"))
  .settings(
    frgaalJavaCompilerSetting,
    version := "0.1",
    libraryDependencies ++= circe ++ Seq(
      "com.typesafe.scala-logging" %% "scala-logging"    % scalaLoggingVersion,
      "commons-io"                  % "commons-io"       % commonsIoVersion,
      "org.apache.commons"          % "commons-compress" % commonsCompressVersion,
      "org.scalatest"              %% "scalatest"        % scalatestVersion % Test,
      akkaActor,
      akkaStream,
      akkaHttp,
      akkaSLF4J
    )
  )
  .dependsOn(cli)

lazy val `edition-updater` = project
  .in(file("lib/scala/edition-updater"))
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    Test / test := (Test / test).tag(simpleLibraryServerTag).value,
    libraryDependencies ++= Seq(
      "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion,
      "org.scalatest"              %% "scalatest"     % scalatestVersion % Test
    )
  )
  .dependsOn(editions)
  .dependsOn(downloader)
  .dependsOn(`distribution-manager`)
  .dependsOn(`library-manager-test` % Test)

lazy val `edition-uploader` = project
  .in(file("lib/scala/edition-uploader"))
  .settings(
    frgaalJavaCompilerSetting
  )
  .dependsOn(editions)
  .dependsOn(`version-output`)

lazy val `library-manager` = project
  .in(file("lib/scala/library-manager"))
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    libraryDependencies ++= Seq(
      "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion,
      "org.scalatest"              %% "scalatest"     % scalatestVersion % Test
    )
  )
  .dependsOn(`version-output`) // Note [Default Editions]
  .dependsOn(editions)
  .dependsOn(cli)
  .dependsOn(`distribution-manager`)
  .dependsOn(downloader)
  .dependsOn(testkit % Test)

lazy val `library-manager-test` = project
  .in(file("lib/scala/library-manager-test"))
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    Test / fork := true,
    Test / javaOptions ++= testLogProviderOptions,
    Test / test := (Test / test).tag(simpleLibraryServerTag).value,
    libraryDependencies ++= Seq(
      "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion,
      "org.scalatest"              %% "scalatest"     % scalatestVersion % Test
    )
  )
  .dependsOn(`library-manager`)
  .dependsOn(`logging-utils` % "test->test")
  .dependsOn(testkit)
  .dependsOn(`logging-service-logback` % "test->test")

lazy val `connected-lock-manager` = project
  .in(file("lib/scala/connected-lock-manager"))
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    libraryDependencies ++= Seq(
      "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion,
      akkaActor,
      akkaTestkit      % Test,
      "org.scalatest" %% "scalatest" % scalatestVersion % Test
    )
  )
  .dependsOn(`distribution-manager`)
  .dependsOn(`polyglot-api`)
  .dependsOn(testkit % Test)

lazy val `runtime-version-manager` = project
  .in(file("lib/scala/runtime-version-manager"))
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    resolvers += Resolver.bintrayRepo("gn0s1s", "releases"),
    libraryDependencies ++= Seq(
      "com.typesafe.scala-logging" %% "scala-logging"    % scalaLoggingVersion,
      "org.typelevel"              %% "cats-core"        % catsVersion,
      "nl.gn0s1s"                  %% "bump"             % bumpVersion,
      "org.apache.commons"          % "commons-compress" % commonsCompressVersion,
      "org.scalatest"              %% "scalatest"        % scalatestVersion % Test,
      akkaHttp
    )
  )
  .dependsOn(pkg)
  .dependsOn(downloader)
  .dependsOn(cli)
  .dependsOn(`version-output`)
  .dependsOn(`edition-updater`)
  .dependsOn(`distribution-manager`)

lazy val `runtime-version-manager-test` = project
  .in(file("lib/scala/runtime-version-manager-test"))
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    libraryDependencies ++= Seq(
      "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion,
      "org.scalatest"              %% "scalatest"     % scalatestVersion,
      "commons-io"                  % "commons-io"    % commonsIoVersion
    )
  )
  .settings(Test / parallelExecution := false)
  .settings(
    (Test / test) := (Test / test)
      .dependsOn(`locking-test-helper` / assembly)
      .value
  )
  .dependsOn(`runtime-version-manager`)
  .dependsOn(testkit)
  .dependsOn(cli)
  .dependsOn(`distribution-manager`)

lazy val `locking-test-helper` = project
  .in(file("lib/scala/locking-test-helper"))
  .settings(
    frgaalJavaCompilerSetting,
    assembly / test := {},
    assembly / assemblyOutputPath := file("locking-test-helper.jar")
  )

val `std-lib-root` = file("distribution/lib/Standard/")
def stdLibComponentRoot(name: String): File =
  `std-lib-root` / name / stdLibVersion
val `base-polyglot-root`  = stdLibComponentRoot("Base") / "polyglot" / "java"
val `table-polyglot-root` = stdLibComponentRoot("Table") / "polyglot" / "java"
val `image-polyglot-root` = stdLibComponentRoot("Image") / "polyglot" / "java"
val `google-api-polyglot-root` =
  stdLibComponentRoot("Google_Api") / "polyglot" / "java"
val `database-polyglot-root` =
  stdLibComponentRoot("Database") / "polyglot" / "java"
val `std-aws-polyglot-root` =
  stdLibComponentRoot("AWS") / "polyglot" / "java"

lazy val `std-base` = project
  .in(file("std-bits") / "base")
  .settings(
    frgaalJavaCompilerSetting,
    autoScalaLibrary := false,
    Compile / compile / compileInputs := (Compile / compile / compileInputs)
      .dependsOn(SPIHelpers.ensureSPIConsistency)
      .value,
    Compile / packageBin / artifactPath :=
      `base-polyglot-root` / "std-base.jar",
    libraryDependencies ++= Seq(
      "org.graalvm.sdk"  % "graal-sdk"               % graalMavenPackagesVersion % "provided",
      "org.netbeans.api" % "org-openide-util-lookup" % netbeansApiVersion        % "provided"
    ),
    Compile / packageBin := Def.task {
      val result = (Compile / packageBin).value
      val _ensureCoreIsCompiled =
        (`common-polyglot-core-utils` / Compile / packageBin).value
      val _ = StdBits
        .copyDependencies(
          `base-polyglot-root`,
          Seq("std-base.jar", "common-polyglot-core-utils.jar"),
          ignoreScalaLibrary = true
        )
        .value
      result
    }.value
  )
  .dependsOn(`common-polyglot-core-utils`)

lazy val `common-polyglot-core-utils` = project
  .in(file("lib/scala/common-polyglot-core-utils"))
  .settings(
    frgaalJavaCompilerSetting,
    autoScalaLibrary := false,
    Compile / packageBin / artifactPath :=
      `base-polyglot-root` / "common-polyglot-core-utils.jar",
    libraryDependencies ++= Seq(
      "com.ibm.icu"     % "icu4j"     % icuVersion,
      "org.graalvm.sdk" % "graal-sdk" % graalMavenPackagesVersion % "provided"
    )
  )

lazy val `enso-test-java-helpers` = project
  .in(file("test/Tests/polyglot-sources/enso-test-java-helpers"))
  .settings(
    frgaalJavaCompilerSetting,
    autoScalaLibrary := false,
    Compile / packageBin / artifactPath :=
      file("test/Tests/polyglot/java/helpers.jar"),
    libraryDependencies ++= Seq(
      "org.graalvm.sdk" % "graal-sdk" % graalMavenPackagesVersion % "provided"
    ),
    Compile / packageBin := Def.task {
      val result          = (Compile / packageBin).value
      val primaryLocation = (Compile / packageBin / artifactPath).value
      val secondaryLocations = Seq(
        file("test/Table_Tests/polyglot/java/helpers.jar")
      )
      secondaryLocations.foreach { target =>
        IO.copyFile(primaryLocation, target)
      }
      result
    }.value
  )
  .dependsOn(`std-base` % "provided")
  .dependsOn(`std-table` % "provided")

lazy val `exploratory-benchmark-java-helpers` = project
  .in(
    file(
      "test/Exploratory_Benchmarks/polyglot-sources/exploratory-benchmark-java-helpers"
    )
  )
  .settings(
    frgaalJavaCompilerSetting,
    autoScalaLibrary := false,
    Compile / packageBin / artifactPath :=
      file(
        "test/Exploratory_Benchmarks/polyglot/java/exploratory-benchmark-java-helpers.jar"
      ),
    libraryDependencies ++= Seq(
      "org.graalvm.sdk" % "graal-sdk" % graalMavenPackagesVersion % "provided"
    )
  )
  .dependsOn(`std-base` % "provided")
  .dependsOn(`std-table` % "provided")

lazy val `benchmark-java-helpers` = project
  .in(
    file(
      "test/Benchmarks/polyglot-sources/benchmark-java-helpers"
    )
  )
  .settings(
    frgaalJavaCompilerSetting,
    autoScalaLibrary := false,
    Compile / packageBin / artifactPath :=
      file(
        "test/Benchmarks/polyglot/java/benchmark-java-helpers.jar"
      ),
    libraryDependencies ++= Seq(
      "org.graalvm.sdk" % "graal-sdk" % graalMavenPackagesVersion % "provided"
    )
  )
  .dependsOn(`std-base` % "provided")
  .dependsOn(`std-table` % "provided")

lazy val `std-table` = project
  .in(file("std-bits") / "table")
  .enablePlugins(Antlr4Plugin)
  .settings(
    frgaalJavaCompilerSetting,
    autoScalaLibrary := false,
    Compile / compile / compileInputs := (Compile / compile / compileInputs)
      .dependsOn(SPIHelpers.ensureSPIConsistency)
      .value,
    Compile / packageBin / artifactPath :=
      `table-polyglot-root` / "std-table.jar",
    Antlr4 / antlr4PackageName := Some("org.enso.table.expressions"),
    Antlr4 / antlr4Version := antlrVersion,
    Antlr4 / antlr4GenVisitor := true,
    Antlr4 / antlr4TreatWarningsAsErrors := true,
    Compile / managedSourceDirectories += {
      (Antlr4 / sourceManaged).value / "main" / "antlr4"
    },
    libraryDependencies ++= Seq(
      "org.graalvm.sdk"          % "graal-sdk"               % graalMavenPackagesVersion % "provided",
      "org.netbeans.api"         % "org-openide-util-lookup" % netbeansApiVersion        % "provided",
      "com.univocity"            % "univocity-parsers"       % univocityParsersVersion,
      "org.apache.poi"           % "poi-ooxml"               % poiOoxmlVersion,
      "org.apache.xmlbeans"      % "xmlbeans"                % xmlbeansVersion,
      "org.antlr"                % "antlr4-runtime"          % antlrVersion,
      "org.apache.logging.log4j" % "log4j-to-slf4j"          % "2.18.0" // org.apache.poi uses log4j
    ),
    Compile / packageBin := Def.task {
      val result = (Compile / packageBin).value
      val _ = StdBits
        .copyDependencies(
          `table-polyglot-root`,
          Seq("std-table.jar"),
          ignoreScalaLibrary = true
        )
        .value
      result
    }.value
  )
  .dependsOn(`std-base` % "provided")

lazy val `std-image` = project
  .in(file("std-bits") / "image")
  .settings(
    frgaalJavaCompilerSetting,
    autoScalaLibrary := false,
    Compile / compile / compileInputs := (Compile / compile / compileInputs)
      .dependsOn(SPIHelpers.ensureSPIConsistency)
      .value,
    Compile / packageBin / artifactPath :=
      `image-polyglot-root` / "std-image.jar",
    libraryDependencies ++= Seq(
      "org.graalvm.sdk"  % "graal-sdk"               % graalMavenPackagesVersion % "provided",
      "org.netbeans.api" % "org-openide-util-lookup" % netbeansApiVersion        % "provided",
      "org.openpnp"      % "opencv"                  % "4.7.0-0"
    ),
    Compile / packageBin := Def.task {
      val result = (Compile / packageBin).value
      val _ = StdBits
        .copyDependencies(
          `image-polyglot-root`,
          Seq("std-image.jar"),
          ignoreScalaLibrary = true
        )
        .value
      result
    }.value
  )
  .dependsOn(`std-base` % "provided")

lazy val `std-google-api` = project
  .in(file("std-bits") / "google-api")
  .settings(
    frgaalJavaCompilerSetting,
    autoScalaLibrary := false,
    Compile / compile / compileInputs := (Compile / compile / compileInputs)
      .dependsOn(SPIHelpers.ensureSPIConsistency)
      .value,
    Compile / packageBin / artifactPath :=
      `google-api-polyglot-root` / "std-google-api.jar",
    libraryDependencies ++= Seq(
      "com.google.api-client" % "google-api-client"          % "1.35.2",
      "com.google.apis"       % "google-api-services-sheets" % "v4-rev612-1.25.0"
    ),
    Compile / packageBin := Def.task {
      val result = (Compile / packageBin).value
      val _ = StdBits
        .copyDependencies(
          `google-api-polyglot-root`,
          Seq("std-google-api.jar"),
          ignoreScalaLibrary = true
        )
        .value
      result
    }.value
  )

lazy val `std-database` = project
  .in(file("std-bits") / "database")
  .settings(
    frgaalJavaCompilerSetting,
    autoScalaLibrary := false,
    Compile / compile / compileInputs := (Compile / compile / compileInputs)
      .dependsOn(SPIHelpers.ensureSPIConsistency)
      .value,
    Compile / packageBin / artifactPath :=
      `database-polyglot-root` / "std-database.jar",
    libraryDependencies ++= Seq(
      "org.graalvm.sdk"  % "graal-sdk"               % graalMavenPackagesVersion % "provided",
      "org.netbeans.api" % "org-openide-util-lookup" % netbeansApiVersion        % "provided",
      "org.xerial"       % "sqlite-jdbc"             % sqliteVersion,
      "org.postgresql"   % "postgresql"              % "42.4.0"
    ),
    Compile / packageBin := Def.task {
      val result = (Compile / packageBin).value
      val _ = StdBits
        .copyDependencies(
          `database-polyglot-root`,
          Seq("std-database.jar"),
          ignoreScalaLibrary = true
        )
        .value
      result
    }.value
  )
  .dependsOn(`std-base` % "provided")
  .dependsOn(`std-table` % "provided")

lazy val `std-aws` = project
  .in(file("std-bits") / "aws")
  .settings(
    frgaalJavaCompilerSetting,
    autoScalaLibrary := false,
    Compile / compile / compileInputs := (Compile / compile / compileInputs)
      .dependsOn(SPIHelpers.ensureSPIConsistency)
      .value,
    Compile / packageBin / artifactPath :=
      `std-aws-polyglot-root` / "std-aws.jar",
    libraryDependencies ++= Seq(
      "org.netbeans.api"       % "org-openide-util-lookup" % netbeansApiVersion % "provided",
      "com.amazon.redshift"    % "redshift-jdbc42"         % redshiftVersion,
      "com.amazonaws"          % "aws-java-sdk-core"       % awsJavaSdkV1Version,
      "com.amazonaws"          % "aws-java-sdk-redshift"   % awsJavaSdkV1Version,
      "com.amazonaws"          % "aws-java-sdk-sts"        % awsJavaSdkV1Version,
      "software.amazon.awssdk" % "auth"                    % awsJavaSdkV2Version,
      "software.amazon.awssdk" % "s3"                      % awsJavaSdkV2Version
    ),
    Compile / packageBin := Def.task {
      val result = (Compile / packageBin).value
      val _ = StdBits
        .copyDependencies(
          `std-aws-polyglot-root`,
          Seq("std-aws.jar"),
          ignoreScalaLibrary = true
        )
        .value
      result
    }.value
  )
  .dependsOn(`std-base` % "provided")
  .dependsOn(`std-table` % "provided")
  .dependsOn(`std-database` % "provided")

/* Note [Native Image Workaround for GraalVM 20.2]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * In GraalVM 20.2 the Native Image build of even simple Scala programs has
 * started to fail on a call to `Statics.releaseFence`. It has been reported as
 * a bug in the GraalVM repository: https://github.com/oracle/graal/issues/2770
 *
 * A proposed workaround for this bug is to substitute the original function
 * with a different implementation that does not use the problematic
 * MethodHandle. This is implemented in class
 * `org.enso.launcher.workarounds.ReplacementStatics` using
 * `org.enso.launcher.workarounds.Unsafe` which gives access to
 * `sun.misc.Unsafe` which contains a low-level function corresponding to the
 * required "release fence".
 *
 * To allow for that substitution, the launcher code requires annotations from
 * the `svm` module and that is why this additional dependency is needed as long
 * as that workaround is in-place. The dependency is marked as "provided"
 * because it is included within the native-image build.
 */

/* Note [WSLoggerManager Shutdown Hook]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * As the WSLoggerManager registers a shutdown hook when its initialized to
 * ensure that logs are not lost in case of logging service initialization
 * failure, it has to be initialized at runtime, as otherwise if the
 * initialization was done at build time, the shutdown hook would actually also
 * run at build time and have no effect at runtime.
 */

lazy val engineDistributionRoot =
  settingKey[File]("Root of built engine distribution")
lazy val launcherDistributionRoot =
  settingKey[File]("Root of built launcher distribution")
lazy val projectManagerDistributionRoot =
  settingKey[File]("Root of built project manager distribution")

engineDistributionRoot :=
  packageBuilder.localArtifact("engine") / s"enso-$ensoVersion"
launcherDistributionRoot := packageBuilder.localArtifact("launcher") / "enso"
projectManagerDistributionRoot :=
  packageBuilder.localArtifact("project-manager") / "enso"

lazy val buildEngineDistribution =
  taskKey[Unit]("Builds the engine distribution")
buildEngineDistribution := {
  val _ = (`engine-runner` / assembly).value
  updateLibraryManifests.value
  val root         = engineDistributionRoot.value
  val log          = streams.value.log
  val cacheFactory = streams.value.cacheStoreFactory
  DistributionPackage.createEnginePackage(
    distributionRoot    = root,
    cacheFactory        = cacheFactory,
    log                 = log,
    graalVersion        = graalMavenPackagesVersion,
    javaVersion         = graalVersion,
    ensoVersion         = ensoVersion,
    editionName         = currentEdition,
    sourceStdlibVersion = stdLibVersion,
    targetStdlibVersion = targetStdlibVersion,
    targetDir           = (`syntax-rust-definition` / rustParserTargetDirectory).value,
    generateIndex       = true
  )
  log.info(s"Engine package created at $root")
}

// This makes the buildEngineDistribution task usable as a dependency
// of other tasks.
ThisBuild / buildEngineDistribution := {
  buildEngineDistribution.result.value
}

lazy val buildEngineDistributionNoIndex =
  taskKey[Unit]("Builds the engine distribution without generating indexes")
buildEngineDistributionNoIndex := {
  val _ = (`engine-runner` / assembly).value
  updateLibraryManifests.value
  val root         = engineDistributionRoot.value
  val log          = streams.value.log
  val cacheFactory = streams.value.cacheStoreFactory
  DistributionPackage.createEnginePackage(
    distributionRoot    = root,
    cacheFactory        = cacheFactory,
    log                 = log,
    graalVersion        = graalMavenPackagesVersion,
    javaVersion         = graalVersion,
    ensoVersion         = ensoVersion,
    editionName         = currentEdition,
    sourceStdlibVersion = stdLibVersion,
    targetStdlibVersion = targetStdlibVersion,
    targetDir           = (`syntax-rust-definition` / rustParserTargetDirectory).value,
    generateIndex       = false
  )
  log.info(s"Engine package created at $root")
}

lazy val runEngineDistribution =
  inputKey[Unit]("Run or --debug the engine distribution with arguments")
runEngineDistribution := {
  buildEngineDistribution.value
  val args: Seq[String] = spaceDelimited("<arg>").parsed
  DistributionPackage.runEnginePackage(
    engineDistributionRoot.value,
    args,
    streams.value.log
  )
}

lazy val runProjectManagerDistribution =
  inputKey[Unit](
    "Run or --debug the project manager distribution with arguments"
  )
runProjectManagerDistribution := {
  buildEngineDistribution.value
  buildProjectManagerDistribution.value
  val args: Seq[String] = spaceDelimited("<arg>").parsed
  DistributionPackage.runProjectManagerPackage(
    engineDistributionRoot.value,
    projectManagerDistributionRoot.value,
    args,
    streams.value.log
  )
}

val allStdBitsSuffix = List("All", "AllWithIndex")
val stdBitsProjects =
  List(
    "AWS",
    "Base",
    "Database",
    "Google_Api",
    "Image",
    "Table"
  ) ++ allStdBitsSuffix
val allStdBits: Parser[String] =
  stdBitsProjects.map(v => v: Parser[String]).reduce(_ | _)

lazy val `simple-httpbin` = project
  .in(file("tools") / "simple-httpbin")
  .settings(
    frgaalJavaCompilerSetting,
    Compile / javacOptions ++= Seq("-XDignore.symbol.file", "-Xlint:all"),
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-text" % commonsTextVersion
    )
  )
  .configs(Test)

lazy val buildStdLib =
  inputKey[Unit]("Build an individual standard library package")
buildStdLib := Def.inputTaskDyn {
  val cmd: String = allStdBits.parsed
  val root: File  = engineDistributionRoot.value
  // Ensure that a complete distribution was built at least once.
  // Because of `if` in the sbt task definition and usage of `streams.value` one has to
  // delegate to another task definition (sbt restriction).
  if ((root / "manifest.yaml").exists) {
    pkgStdLibInternal.toTask(cmd)
  } else buildEngineDistribution
}.evaluated

lazy val pkgStdLibInternal = inputKey[Unit]("Use `buildStdLib`")
pkgStdLibInternal := Def.inputTask {
  val cmd               = allStdBits.parsed
  val root              = engineDistributionRoot.value
  val log: sbt.Logger   = streams.value.log
  val cacheFactory      = streams.value.cacheStoreFactory
  val standardNamespace = "Standard"
  val buildAllCmd       = allStdBitsSuffix.contains(cmd)
  cmd match {
    case "Base" =>
      (`std-base` / Compile / packageBin).value
    case "Database" =>
      (`std-database` / Compile / packageBin).value
    case "Google_Api" =>
      (`std-google-api` / Compile / packageBin).value
    case "Image" =>
      (`std-image` / Compile / packageBin).value
    case "Table" =>
      (`std-table` / Compile / packageBin).value
    case "TestHelpers" =>
      (`enso-test-java-helpers` / Compile / packageBin).value
      (`exploratory-benchmark-java-helpers` / Compile / packageBin).value
      (`benchmark-java-helpers` / Compile / packageBin).value
    case "AWS" =>
      (`std-aws` / Compile / packageBin).value
    case _ if buildAllCmd =>
      (`std-base` / Compile / packageBin).value
      (`enso-test-java-helpers` / Compile / packageBin).value
      (`exploratory-benchmark-java-helpers` / Compile / packageBin).value
      (`benchmark-java-helpers` / Compile / packageBin).value
      (`std-table` / Compile / packageBin).value
      (`std-database` / Compile / packageBin).value
      (`std-image` / Compile / packageBin).value
      (`std-google-api` / Compile / packageBin).value
      (`std-aws` / Compile / packageBin).value
    case _ =>
  }
  val libs =
    if (!buildAllCmd) Seq(cmd)
    else {
      val prefix = s"$standardNamespace."
      Editions.standardLibraries
        .filter(_.startsWith(prefix))
        .map(_.stripPrefix(prefix))
    }
  val generateIndex = cmd.endsWith("WithIndex")
  libs.foreach { lib =>
    StdBits.buildStdLibPackage(
      lib,
      root,
      cacheFactory,
      log,
      defaultDevEnsoVersion
    )
    if (generateIndex) {
      val stdlibStandardRoot = root / "lib" / standardNamespace
      DistributionPackage.indexStdLib(
        libMajor       = stdlibStandardRoot,
        libName        = stdlibStandardRoot / lib,
        stdLibVersion  = defaultDevEnsoVersion,
        ensoVersion    = defaultDevEnsoVersion,
        ensoExecutable = root / "bin" / "enso",
        cacheFactory   = cacheFactory.sub("stdlib"),
        log            = log
      )
    }
  }
}.evaluated

lazy val buildLauncherDistribution =
  taskKey[Unit]("Builds the launcher distribution")
buildLauncherDistribution := {
  val _            = (launcher / buildNativeImage).value
  val root         = launcherDistributionRoot.value
  val log          = streams.value.log
  val cacheFactory = streams.value.cacheStoreFactory
  DistributionPackage.createLauncherPackage(root, cacheFactory)
  log.info(s"Launcher package created at $root")
}

lazy val buildProjectManagerDistribution =
  taskKey[Unit]("Builds the project manager distribution")
buildProjectManagerDistribution := {
  val _            = (`project-manager` / buildNativeImage).value
  val root         = projectManagerDistributionRoot.value
  val log          = streams.value.log
  val cacheFactory = streams.value.cacheStoreFactory
  DistributionPackage.createProjectManagerPackage(root, cacheFactory)
  log.info(s"Project Manager package created at $root")
}

lazy val buildGraalDistribution =
  taskKey[Unit]("Builds the GraalVM distribution")
buildGraalDistribution := {
  val log    = streams.value.log
  val distOs = "DIST_OS"
  val osName = "os.name"
  val distName = sys.env.get(distOs).getOrElse {
    val name = sys.props(osName).takeWhile(!_.isWhitespace)
    if (sys.env.contains("CI")) {
      log.warn(
        s"$distOs env var is empty. Fallback to system property $osName=$name."
      )
    }
    name
  }
  val os = DistributionPackage.OS(distName).getOrElse {
    throw new RuntimeException(s"Failed to determine OS: $distName.")
  }
  packageBuilder.createGraalPackage(
    log,
    os,
    DistributionPackage.Architecture.X64
  )
}

lazy val updateLibraryManifests =
  taskKey[Unit](
    "Recomputes dependencies to update manifests bundled with libraries."
  )
updateLibraryManifests := {
  val _            = (`engine-runner` / assembly).value
  val log          = streams.value.log
  val cacheFactory = streams.value.cacheStoreFactory
  val libraries = Editions.standardLibraries.map(libName =>
    BundledLibrary(libName, stdLibVersion)
  )

  LibraryManifestGenerator.generateManifests(
    libraries,
    file("distribution"),
    log,
    cacheFactory
  )
}
