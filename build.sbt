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

// This import is unnecessary, but bit adds a proper code completion features
// to IntelliJ.
import JPMSPlugin.autoImport._

import java.io.File
import java.nio.file.Files
import java.nio.file.Paths

// ============================================================================
// === Global Configuration ===================================================
// ============================================================================

val scalacVersion = "2.13.11"
// source version of the Java language
val javaVersion = "21"
// version of the GraalVM JDK
val graalVersion = "21.0.2"
// Version used for the Graal/Truffle related Maven packages
// Keep in sync with GraalVM.version. Do not change the name of this variable,
// it is used by the Rust build script via regex matching.
val graalMavenPackagesVersion = "24.0.0"
val targetJavaVersion         = "17"
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
val mavenUploadVersion  = "0.2-SNAPSHOT"

// Inspired by https://www.scala-sbt.org/1.x/docs/Howto-Startup.html#How+to+take+an+action+on+startup
lazy val startupStateTransition: State => State = { s: State =>
  GraalVM.versionCheck(
    graalVersion,
    graalMavenPackagesVersion,
    javaVersion,
    s
  )
}
Global / onLoad := {
  val old = (Global / onLoad).value
  startupStateTransition compose old
}

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
ThisBuild / publish / skip := true

/* Tag limiting the concurrent access to tools/simple-library-server in tests.
 */
val simpleLibraryServerTag = Tags.Tag("simple-library-server")
Global / concurrentRestrictions += Tags.limit(simpleLibraryServerTag, 1)

lazy val gatherLicenses =
  taskKey[Unit](
    "Gathers licensing information for relevant dependencies of all distributions"
  )
gatherLicenses := {
  val _ = GatherLicenses.run.toTask("").value
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
  makeStdLibDistribution("AWS", Distribution.sbtProjects(`std-aws`)),
  makeStdLibDistribution(
    "Snowflake",
    Distribution.sbtProjects(`std-snowflake`)
  ),
  makeStdLibDistribution(
    "Microsoft",
    Distribution.sbtProjects(`std-microsoft`)
  ),
  makeStdLibDistribution("Tableau", Distribution.sbtProjects(`std-tableau`))
)

GatherLicenses.licenseConfigurations := Set("compile")
GatherLicenses.configurationRoot := file("tools/legal-review")

lazy val openLegalReviewReport =
  inputKey[Unit](
    "Gathers licensing information for relevant dependencies and opens the " +
    "report in review mode in the browser. Specify names of distributions to process, separated by spaces. If no names are provided, all distributions are processed."
  )
openLegalReviewReport := {
  GatherLicenses.run.evaluated
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
  "-encoding",        // Provide explicit encoding (the next line)
  "UTF-8",            // Specify character encoding used by Java source files
  "-deprecation",     // Shows a description of each use or override of a deprecated member or class
  "-g",               // Include debugging information
  "-Xlint:unchecked", // Enable additional warnings
  "-proc:full"        // Annotation processing is enabled
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
  Seq(
    Tests.Argument(TestFrameworks.ScalaTest, "-oID"),
    Tests.Argument(TestFrameworks.JUnit, "--verbosity=1")
  ) ++
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

// ============================================================================
// === Global Project =========================================================
// ============================================================================

lazy val enso = (project in file("."))
  .settings(version := "0.1")
  .aggregate(
    `persistance-dsl`,
    `persistance`,
    `interpreter-dsl`,
    `interpreter-dsl-test`,
    `json-rpc-server-test`,
    `json-rpc-server`,
    `language-server`,
    `polyglot-api`,
    `polyglot-api-macros`,
    `project-manager`,
    `syntax-rust-definition`,
    `text-buffer`,
    yaml,
    `scala-yaml`,
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
    `engine-runner-common`,
    `engine-runner`,
    runtime,
    searcher,
    launcher,
    downloader,
    `runtime-integration-tests`,
    `runtime-benchmarks`,
    `runtime-parser`,
    `runtime-compiler`,
    `runtime-fat-jar`,
    `runtime-suggestions`,
    `runtime-language-epb`,
    `runtime-language-arrow`,
    `runtime-instrument-common`,
    `runtime-instrument-id-execution`,
    `runtime-instrument-repl-debugger`,
    `runtime-instrument-runtime-server`,
    `runtime-version-manager`,
    `runtime-version-manager-test`,
    editions,
    semver,
    `distribution-manager`,
    `edition-updater`,
    `edition-uploader`,
    `library-manager`,
    `library-manager-test`,
    `connected-lock-manager`,
    `connected-lock-manager-server`,
    `process-utils`,
    testkit,
    `test-utils`,
    `common-polyglot-core-utils`,
    `std-base`,
    `std-database`,
    `std-google-api`,
    `std-image`,
    `std-table`,
    `std-aws`,
    `std-snowflake`,
    `std-microsoft`,
    `std-tableau`,
    `http-test-helper`,
    `enso-test-java-helpers`,
    `exploratory-benchmark-java-helpers`,
    `benchmark-java-helpers`,
    `benchmarks-common`,
    `bench-processor`,
    `ydoc-server`,
    `desktop-environment`
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
val logbackClassicVersion     = JPMSUtils.logbackClassicVersion
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

val circeVersion              = "0.14.7"
val circeGenericExtrasVersion = "0.14.3"
val circe = Seq("circe-core", "circe-generic", "circe-parser")
  .map("io.circe" %% _ % circeVersion)
val snakeyamlVersion = "2.2"

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

// === Helidon ================================================================
val jakartaVersion = "2.0.1"
val helidonVersion = "3.2.9"
val helidon = Seq(
  "io.helidon.common"    % "helidon-common"                     % helidonVersion,
  "io.helidon.common"    % "helidon-common-configurable"        % helidonVersion,
  "io.helidon.common"    % "helidon-common-context"             % helidonVersion,
  "io.helidon.common"    % "helidon-common-http"                % helidonVersion,
  "io.helidon.common"    % "helidon-common-key-util"            % helidonVersion,
  "io.helidon.common"    % "helidon-common-mapper"              % helidonVersion,
  "io.helidon.common"    % "helidon-common-media-type"          % helidonVersion,
  "io.helidon.common"    % "helidon-common-reactive"            % helidonVersion,
  "io.helidon.common"    % "helidon-common-service-loader"      % helidonVersion,
  "io.helidon.config"    % "helidon-config"                     % helidonVersion,
  "io.helidon.logging"   % "helidon-logging-common"             % helidonVersion,
  "io.helidon.media"     % "helidon-media-common"               % helidonVersion,
  "io.helidon.tracing"   % "helidon-tracing"                    % helidonVersion,
  "io.helidon.tracing"   % "helidon-tracing-config"             % helidonVersion,
  "io.helidon.webserver" % "helidon-webserver"                  % helidonVersion,
  "io.helidon.webserver" % "helidon-webserver-websocket"        % helidonVersion,
  "jakarta.activation"   % "jakarta.activation-api"             % "2.1.2",
  "jakarta.annotation"   % "jakarta.annotation-api"             % "2.0.0",
  "jakarta.websocket"    % "jakarta.websocket-api"              % "2.1.1",
  "jakarta.websocket"    % "jakarta.websocket-client-api"       % "2.1.1",
  "jakarta.xml.bind"     % "jakarta.xml.bind-api"               % "4.0.1",
  "io.netty"             % "netty-buffer"                       % "4.1.108.Final",
  "io.netty"             % "netty-codec"                        % "4.1.108.Final",
  "io.netty"             % "netty-codec-http"                   % "4.1.108.Final",
  "io.netty"             % "netty-common"                       % "4.1.108.Final",
  "io.netty"             % "netty-handler"                      % "4.1.108.Final",
  "io.netty"             % "netty-resolver"                     % "4.1.108.Final",
  "io.netty"             % "netty-transport"                    % "4.1.108.Final",
  "io.netty"             % "netty-transport-native-unix-common" % "4.1.108.Final",
  "org.glassfish.tyrus"  % "tyrus-client"                       % "2.1.5",
  "org.glassfish.tyrus"  % "tyrus-container-jdk-client"         % "2.1.5",
  "org.glassfish.tyrus"  % "tyrus-core"                         % "2.1.5",
  "org.glassfish.tyrus"  % "tyrus-server"                       % "2.1.5",
  "org.glassfish.tyrus"  % "tyrus-spi"                          % "2.1.5"
)

val helidon4Version = "4.0.8"
val helidon4 = Seq(
  "io.helidon"                 % "helidon"                     % helidonVersion,
  "io.helidon.builder"         % "helidon-builder-api"         % helidonVersion,
  "io.helidon.common"          % "helidon-common"              % helidonVersion,
  "io.helidon.common"          % "helidon-common-buffers"      % helidonVersion,
  "io.helidon.common"          % "helidon-common-config"       % helidonVersion,
  "io.helidon.common"          % "helidon-common-configurable" % helidonVersion,
  "io.helidon.common"          % "helidon-common-context"      % helidonVersion,
  "io.helidon.common"          % "helidon-common-key-util"     % helidonVersion,
  "io.helidon.common"          % "helidon-common-mapper"       % helidonVersion,
  "io.helidon.common"          % "helidon-common-media-type"   % helidonVersion,
  "io.helidon.common"          % "helidon-common-parameters"   % helidonVersion,
  "io.helidon.common"          % "helidon-common-socket"       % helidonVersion,
  "io.helidon.common"          % "helidon-common-security"     % helidonVersion,
  "io.helidon.common"          % "helidon-common-task"         % helidonVersion,
  "io.helidon.common"          % "helidon-common-types"        % helidonVersion,
  "io.helidon.common"          % "helidon-common-tls"          % helidonVersion,
  "io.helidon.common"          % "helidon-common-uri"          % helidonVersion,
  "io.helidon.common.features" % "helidon-common-features"     % helidonVersion,
  "io.helidon.common.features" % "helidon-common-features-api" % helidonVersion,
  "io.helidon.config"          % "helidon-config"              % helidonVersion,
  "io.helidon.logging"         % "helidon-logging-common"      % helidonVersion,
  "io.helidon.inject"          % "helidon-inject-api"          % helidonVersion,
  "io.helidon.http"            % "helidon-http"                % helidonVersion,
  "io.helidon.http.encoding"   % "helidon-http-encoding"       % helidonVersion,
  "io.helidon.http.media"      % "helidon-http-media"          % helidonVersion,
  "io.helidon.webserver"       % "helidon-webserver"           % helidonVersion,
  "io.helidon.webserver"       % "helidon-webserver-websocket" % helidonVersion,
  "io.helidon.websocket"       % "helidon-websocket"           % helidonVersion,
  "jakarta.inject"             % "jakarta.inject-api"          % jakartaVersion
)

// === Jackson ================================================================

val jacksonVersion = "2.15.2"

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
val scalaCollectionCompatVersion = "2.8.1"

// === std-lib ================================================================

val antlrVersion            = "4.13.0"
val awsJavaSdkV1Version     = "1.12.480"
val awsJavaSdkV2Version     = "2.25.36"
val icuVersion              = "73.1"
val poiOoxmlVersion         = "5.2.3"
val redshiftVersion         = "2.1.0.15"
val univocityParsersVersion = "2.9.1"
val xmlbeansVersion         = "5.1.1"
val tableauVersion          = "0.0.19691.r2d7e5bc8"

// === ZIO ====================================================================

val zioVersion            = "2.0.14"
val zioInteropCatsVersion = "23.0.0.6"
val zio = Seq(
  "dev.zio" %% "zio"              % zioVersion,
  "dev.zio" %% "zio-interop-cats" % zioInteropCatsVersion
)

// === Other ==================================================================

val bcpkixJdk15Version      = "1.70"
val declineVersion          = "2.4.1"
val diffsonVersion          = "4.4.0"
val directoryWatcherVersion = "0.18.0"
val flatbuffersVersion      = "24.3.25"
val guavaVersion            = "32.0.0-jre"
val jlineVersion            = "3.23.0"
val jgitVersion             = "6.7.0.202309050840-r"
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
val slf4jVersion            = JPMSUtils.slf4jVersion
val sqliteVersion           = "3.42.0.0"
val tikaVersion             = "2.4.1"
val typesafeConfigVersion   = "1.4.2"
val junitVersion            = "4.13.2"
val junitIfVersion          = "0.13.2"
val hamcrestVersion         = "1.3"
val netbeansApiVersion      = "RELEASE180"
val fansiVersion            = "0.4.0"
val httpComponentsVersion   = "4.4.1"
val apacheArrowVersion      = "14.0.1"
val snowflakeJDBCVersion    = "3.15.0"
val mssqlserverJDBCVersion  = "12.6.2.jre11"
val jsoniterVersion         = "2.28.5"
val jnaVersion              = "5.14.0"

// ============================================================================
// === Utility methods =====================================================
// ============================================================================

lazy val componentModulesIds =
  taskKey[Seq[ModuleID]](
    "Gather all sbt module IDs that will be put on the module-path for the engine runner"
  )
(ThisBuild / componentModulesIds) := {
  GraalVM.modules ++ GraalVM.langsPkgs ++ GraalVM.toolsPkgs ++ helidon ++ Seq(
    "org.slf4j"        % "slf4j-api"                    % slf4jVersion,
    "ch.qos.logback"   % "logback-classic"              % logbackClassicVersion,
    "ch.qos.logback"   % "logback-core"                 % logbackClassicVersion,
    "org.netbeans.api" % "org-netbeans-modules-sampler" % netbeansApiVersion,
    (`runtime-language-arrow` / projectID).value,
    (`syntax-rust-definition` / projectID).value,
    (`ydoc-server` / projectID).value,
    (`profiling-utils` / projectID).value,
    (`runtime-fat-jar` / projectID).value
  )
}

// TODO[pm]: this is now deprecated and should be removed
lazy val componentModulesPaths =
  taskKey[Seq[File]](
    "Gathers all component modules (Jar archives that should be put on module-path" +
    " as files"
  )
(ThisBuild / componentModulesPaths) := {
  val runnerCp  = (`engine-runner` / Runtime / fullClasspath).value
  val runtimeCp = (LocalProject("runtime") / Runtime / fullClasspath).value
  val fullCp    = (runnerCp ++ runtimeCp).distinct
  val log       = streams.value.log
  val thirdPartyModIds =
    GraalVM.modules ++
    GraalVM.langsPkgs ++
    GraalVM.toolsPkgs ++
    helidon ++
    Seq(
      "org.slf4j"        % "slf4j-api"                    % slf4jVersion,
      "ch.qos.logback"   % "logback-classic"              % logbackClassicVersion,
      "ch.qos.logback"   % "logback-core"                 % logbackClassicVersion,
      "org.netbeans.api" % "org-netbeans-modules-sampler" % netbeansApiVersion
    )
  val thirdPartyMods = JPMSUtils.filterModulesFromClasspath(
    fullCp,
    thirdPartyModIds,
    log,
    shouldContainAll = true
  )
  val thirdPartyModFiles = thirdPartyMods.map(_.data)
  val arrow              = (`runtime-language-arrow` / Compile / packageBin).value
  val syntax             = (`syntax-rust-definition` / Compile / packageBin).value
  val ydoc               = (`ydoc-server` / Compile / packageBin).value
  val profilingUtils     = (`profiling-utils` / Compile / packageBin).value
  val runtime            = (`runtime-fat-jar` / assembly / assemblyOutputPath).value
  val ourMods = Seq(
    runtime,
    arrow,
    syntax,
    ydoc,
    profilingUtils
  )
  ourMods ++ thirdPartyModFiles
}

lazy val compileModuleInfo = taskKey[Unit]("Compiles `module-info.java`")

// ============================================================================
// === Internal Libraries =====================================================
// ============================================================================

lazy val `text-buffer` = project
  .in(file("lib/scala/text-buffer"))
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    commands += WithDebugCommand.withDebug,
    libraryDependencies ++= Seq(
      "org.scalatest"  %% "scalatest"  % scalatestVersion  % Test,
      "org.scalacheck" %% "scalacheck" % scalacheckVersion % Test
    )
  )

lazy val rustParserTargetDirectory =
  SettingKey[File]("target directory for the Rust parser")

(`syntax-rust-definition` / rustParserTargetDirectory) := {
  target.value / "rust" / "parser-jni"
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
    val target = os.toLowerCase() match {
      case DistributionPackage.OS.Linux.name =>
        Some("x86_64-unknown-linux-musl")
      case _ =>
        None
    }
    target.foreach { t =>
      Cargo.rustUp(t, log)
    }
    val profile = if (BuildInfo.isReleaseMode) "release" else "fuzz"
    val arguments = Seq(
      "build",
      "-p",
      "enso-parser-jni",
      "--profile",
      profile,
      "-Z",
      "unstable-options"
    ) ++ target.map(t => Seq("--target", t)).getOrElse(Seq()) ++
      Seq(
        "--out-dir",
        (`syntax-rust-definition` / rustParserTargetDirectory).value.toString
      )
    val envVars = target
      .map(_ => Seq(("RUSTFLAGS", "-C target-feature=-crt-static")))
      .getOrElse(Seq())
    Cargo.run(arguments, log, envVars)
  }
  FileTreeView.default.list(Seq(libGlob)).map(_._1.toFile)
}

`syntax-rust-definition` / generateRustParserLib / fileInputs +=
  (`syntax-rust-definition` / baseDirectory).value.toGlob / "jni" / "src" / ** / "*.rs"
`syntax-rust-definition` / generateRustParserLib / fileInputs +=
  (`syntax-rust-definition` / baseDirectory).value.toGlob / "src" / ** / "*.rs"

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
  .enablePlugins(JPMSPlugin)
  .configs(Test)
  .settings(
    version := mavenUploadVersion,
    Compile / exportJars := true,
    javadocSettings,
    publish / skip := false,
    autoScalaLibrary := false,
    crossPaths := false,
    javaModuleName := "org.enso.syntax",
    Compile / sourceGenerators += generateParserJavaSources,
    Compile / resourceGenerators += generateRustParserLib,
    Compile / javaSource := baseDirectory.value / "generate-java" / "java",
    Compile / compile / javacOptions ++= Seq("-source", "11", "-target", "11")
  )

lazy val yaml = (project in file("lib/java/yaml"))
  .settings(
    frgaalJavaCompilerSetting,
    version := "0.1",
    libraryDependencies ++= Seq(
      "org.yaml" % "snakeyaml" % snakeyamlVersion % "provided"
    )
  )

lazy val `scala-yaml` = (project in file("lib/scala/yaml"))
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    libraryDependencies ++= Seq(
      "org.yaml" % "snakeyaml" % snakeyamlVersion % "provided"
    )
  )

lazy val pkg = (project in file("lib/scala/pkg"))
  .settings(
    Compile / run / mainClass := Some("org.enso.pkg.Main"),
    frgaalJavaCompilerSetting,
    version := "0.1",
    libraryDependencies ++= Seq(
      "org.graalvm.truffle" % "truffle-api"      % graalMavenPackagesVersion % "provided",
      "io.circe"           %% "circe-core"       % circeVersion              % "provided",
      "org.yaml"            % "snakeyaml"        % snakeyamlVersion          % "provided",
      "org.scalatest"      %% "scalatest"        % scalatestVersion          % Test,
      "org.apache.commons"  % "commons-compress" % commonsCompressVersion
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
  .enablePlugins(JPMSPlugin)
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    compileOrder := CompileOrder.JavaThenScala,
    javaModuleName := "org.enso.profiling",
    Compile / exportJars := true,
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
      exclude ("org.netbeans.api", "org-netbeans-api-annotations-common"),
      "org.slf4j"      % "slf4j-api"       % slf4jVersion,
      "junit"          % "junit"           % junitVersion   % Test,
      "com.github.sbt" % "junit-interface" % junitIfVersion % Test
    ),
    moduleDependencies := {
      Seq(
        "org.slf4j"        % "slf4j-api"                    % slf4jVersion,
        "org.netbeans.api" % "org-netbeans-modules-sampler" % netbeansApiVersion
      )
    }
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
      "org.scalatest" %% "scalatest" % scalatestVersion % Test
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
      "org.scalatest"   %% "scalatest"               % scalatestVersion   % Test,
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
      "org.yaml"                    % "snakeyaml"     % snakeyamlVersion % "provided",
      "org.scalatest"              %% "scalatest"     % scalatestVersion % Test
    ),
    Test / parallelExecution := false
  )
  .dependsOn(`scala-yaml`)

lazy val `task-progress-notifications` = project
  .in(file("lib/scala/task-progress-notifications"))
  .configs(Test)
  .settings(
    version := "0.1",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % scalatestVersion % Test
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
  .enablePlugins(JPMSPlugin)
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
    libraryDependencies ++= circe ++ helidon,
    libraryDependencies ++= Seq(
      "com.typesafe"                % "config"                       % typesafeConfigVersion,
      "com.github.pureconfig"      %% "pureconfig"                   % pureconfigVersion,
      "com.typesafe.scala-logging" %% "scala-logging"                % scalaLoggingVersion,
      "dev.zio"                    %% "zio"                          % zioVersion,
      "dev.zio"                    %% "zio-interop-cats"             % zioInteropCatsVersion,
      "commons-cli"                 % "commons-cli"                  % commonsCliVersion,
      "commons-io"                  % "commons-io"                   % commonsIoVersion,
      "org.apache.commons"          % "commons-lang3"                % commonsLangVersion,
      "com.miguno.akka"            %% "akka-mock-scheduler"          % akkaMockSchedulerVersion % Test,
      "org.mockito"                %% "mockito-scala"                % mockitoScalaVersion      % Test,
      "junit"                       % "junit"                        % junitVersion             % Test,
      "com.github.sbt"              % "junit-interface"              % junitIfVersion           % Test,
      "org.hamcrest"                % "hamcrest-all"                 % hamcrestVersion          % Test,
      "org.netbeans.api"            % "org-netbeans-modules-sampler" % netbeansApiVersion       % Test
    ),
    addCompilerPlugin(
      "org.typelevel" %% "kind-projector" % kindProjectorVersion cross CrossVersion.full
    )
  )
  /** Fat jar assembly settings
    */
  .settings(
    assembly / assemblyJarName := "project-manager.jar",
    assembly / test := {},
    assembly / assemblyOutputPath := file("project-manager.jar"),
    // Exclude all the Truffle/Graal related artifacts from the fat jar
    assembly / assemblyExcludedJars := {
      val pkgsToExclude = GraalVM.modules
      val ourFullCp     = (Runtime / fullClasspath).value
      JPMSUtils.filterModulesFromClasspath(
        ourFullCp,
        pkgsToExclude,
        streams.value.log
      )
    },
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", file, xs @ _*) if file.endsWith(".DSA") =>
        MergeStrategy.discard
      case PathList("META-INF", file, xs @ _*) if file.endsWith(".SF") =>
        MergeStrategy.discard
      case PathList("META-INF", "MANIFEST.MF", xs @ _*) =>
        MergeStrategy.discard
      // This fat Jar must not be an explicit module, so discard all the module-info classes
      case PathList(xs @ _*) if xs.last.contains("module-info") =>
        MergeStrategy.discard
      case "application.conf" => MergeStrategy.concat
      case "reference.conf"   => MergeStrategy.concat
      case _                  => MergeStrategy.first
    }
  )
  /** JPMS related settings for tests
    */
  .settings(
    Test / fork := true,
    // These dependencies are here so that we can use them in `--module-path` later on.
    libraryDependencies ++= {
      val necessaryModules =
        GraalVM.modules.map(_.withConfigurations(Some(Test.name))) ++
        GraalVM.langsPkgs.map(_.withConfigurations(Some(Test.name)))
      necessaryModules
    },
    Test / addModules := Seq(
      (`runtime-fat-jar` / javaModuleName).value,
      (`syntax-rust-definition` / javaModuleName).value,
      (`profiling-utils` / javaModuleName).value,
      (`ydoc-server` / javaModuleName).value
    ),
    Test / moduleDependencies := {
      GraalVM.modules ++ GraalVM.langsPkgs ++ logbackPkg ++ helidon ++ Seq(
        "org.slf4j"        % "slf4j-api"                    % slf4jVersion,
        "org.netbeans.api" % "org-netbeans-modules-sampler" % netbeansApiVersion,
        (`runtime-fat-jar` / projectID).value,
        (`ydoc-server` / projectID).value,
        (`syntax-rust-definition` / projectID).value,
        (`profiling-utils` / projectID).value
      )
    },
    Test / javaOptions ++= testLogProviderOptions
  )
  .settings(
    NativeImage.smallJdk := None,
    NativeImage.additionalCp := Seq.empty,
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
  .dependsOn(`desktop-environment`)
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
  .dependsOn(`logging-service-logback` % "test->test")
  .dependsOn(`runtime-fat-jar` % Test)
  .dependsOn(`ydoc-server` % Test)
  .dependsOn(`profiling-utils` % Test)

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
    frgaalJavaCompilerSetting,
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
      "org.apache.commons" % "commons-lang3"   % commonsLangVersion,
      "commons-io"         % "commons-io"      % commonsIoVersion,
      "org.scalatest"     %% "scalatest"       % scalatestVersion,
      "junit"              % "junit"           % junitVersion,
      "com.github.sbt"     % "junit-interface" % junitIfVersion
    )
  )

lazy val searcher = project
  .in(file("lib/scala/searcher"))
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    annotationProcSetting,
    libraryDependencies ++= jmh ++ Seq(
      "org.scalatest" %% "scalatest" % scalatestVersion % Test
    ) ++ logbackTest
  )
  .configs(Benchmark)
  .settings(
    inConfig(Benchmark)(Defaults.testSettings),
    Benchmark / fork := true
  )
  .dependsOn(testkit % Test)
  .dependsOn(`polyglot-api`)

lazy val `ydoc-server` = project
  .in(file("lib/java/ydoc-server"))
  .enablePlugins(JPMSPlugin)
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    javaModuleName := "org.enso.ydoc",
    Compile / exportJars := true,
    crossPaths := false,
    autoScalaLibrary := false,
    Test / fork := true,
    commands += WithDebugCommand.withDebug,
    moduleDependencies := {
      helidon ++ Seq(
        "org.graalvm.polyglot" % "polyglot"        % graalMavenPackagesVersion,
        "org.graalvm.truffle"  % "truffle-api"     % graalMavenPackagesVersion,
        "org.slf4j"            % "slf4j-api"       % slf4jVersion,
        "ch.qos.logback"       % "logback-classic" % logbackClassicVersion,
        "ch.qos.logback"       % "logback-core"    % logbackClassicVersion,
        (`syntax-rust-definition` / projectID).value,
        (`profiling-utils` / projectID).value
      ),
    },
    Runtime / moduleDependencies ++=
      GraalVM.modules ++ GraalVM.jsPkgs ++ GraalVM.chromeInspectorPkgs,
    libraryDependencies ++= Seq(
      "org.graalvm.truffle"        % "truffle-api"                 % graalMavenPackagesVersion % "provided",
      "org.graalvm.polyglot"       % "inspect"                     % graalMavenPackagesVersion % "runtime",
      "org.graalvm.polyglot"       % "js"                          % graalMavenPackagesVersion % "runtime",
      "org.slf4j"                  % "slf4j-api"                   % slf4jVersion,
      "io.helidon.webserver"       % "helidon-webserver-websocket" % helidonVersion,
      "junit"                      % "junit"                       % junitVersion              % Test,
      "com.github.sbt"             % "junit-interface"             % junitIfVersion            % Test,
      "com.fasterxml.jackson.core" % "jackson-databind"            % jacksonVersion            % Test,
      "io.helidon.webclient"       % "helidon-webclient-websocket" % helidon4Version           % Test
    )
  )
  // `Compile/run` settings are necessary for the `run` task to work.
  // We add it here for convenience so that one can start ydoc-server directly
  // with `ydoc-server/run` task.
  .settings(
    Compile / run / fork := true,
    Compile / run / connectInput := true,
    Compile / run / javaOptions := Seq(
      "-ea"
    ),
    // We need to assembly the cmd line options here manually, because we need
    // to add path to this module, and adding that directly to the `modulePath` setting
    // would result in an sbt caught in an infinite recursion.
    //
    Compile / run / javaOptions ++= {
      val mp =
        (Runtime / modulePath).value ++ modulePath.value ++ (`profiling-utils` / modulePath).value
      val jar       = (Compile / exportedProductJars).value.head
      val modName   = javaModuleName.value
      val allMp     = mp ++ Seq(jar.data.absolutePath)
      val mainKlazz = (Compile / mainClass).value.get
      val args = Seq(
        "--module-path",
        allMp.mkString(File.pathSeparator),
        "--module",
        modName + "/" + mainKlazz
      )
      args
    },
    Compile / resourceGenerators +=
      Def
        .task(
          Ydoc.generateJsBundle(
            (ThisBuild / baseDirectory).value,
            baseDirectory.value,
            (Compile / resourceManaged).value,
            streams.value
          )
        )
        .taskValue
  )
  .dependsOn(`syntax-rust-definition`)
  .dependsOn(`logging-service-logback`)
  .dependsOn(`profiling-utils`)

lazy val `persistance` = (project in file("lib/java/persistance"))
  .settings(
    version := mavenUploadVersion,
    Test / fork := true,
    commands += WithDebugCommand.withDebug,
    frgaalJavaCompilerSetting,
    annotationProcSetting,
    javadocSettings,
    publish / skip := false,
    autoScalaLibrary := false,
    crossPaths := false,
    Compile / javacOptions := ((Compile / javacOptions).value),
    inConfig(Compile)(truffleRunOptionsSettings),
    libraryDependencies ++= Seq(
      "org.slf4j"        % "slf4j-api"               % slf4jVersion,
      "org.netbeans.api" % "org-openide-util-lookup" % netbeansApiVersion,
      "junit"            % "junit"                   % junitVersion   % Test,
      "com.github.sbt"   % "junit-interface"         % junitIfVersion % Test
    )
  )
  .dependsOn(`persistance-dsl` % Test)

lazy val `persistance-dsl` = (project in file("lib/java/persistance-dsl"))
  .settings(
    version := mavenUploadVersion,
    frgaalJavaCompilerSetting,
    publish / skip := false,
    autoScalaLibrary := false,
    crossPaths := false,
    javadocSettings,
    Compile / compile / javacOptions := ((Compile / compile / javacOptions).value ++
    // Only run ServiceProvider processor and ignore those defined in META-INF, thus
    // fixing incremental compilation setup
    Seq(
      "-processor",
      "org.netbeans.modules.openide.util.ServiceProviderProcessor"
    )),
    libraryDependencies ++= Seq(
      "org.netbeans.api" % "org-openide-util-lookup" % netbeansApiVersion % "provided"
    )
  )

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
      "org.netbeans.api"   % "org-openide-util-lookup" % netbeansApiVersion % "provided",
      "com.google.guava"   % "guava"                   % guavaVersion exclude ("com.google.code.findbugs", "jsr305")
    )
  )

lazy val `interpreter-dsl-test` =
  (project in file("engine/interpreter-dsl-test"))
    .configs(Test)
    .settings(
      version := "0.1",
      frgaalJavaCompilerSetting,
      annotationProcSetting,
      inConfig(Test)(truffleRunOptionsSettings),
      Test / fork := true,
      Test / javaOptions ++= Seq(
        "-Dpolyglotimpl.DisableClassPathIsolation=true"
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
    .dependsOn(`test-utils`)

// ============================================================================
// === Sub-Projects ===========================================================
// ============================================================================

val benchOnlyOptions = if (java.lang.Boolean.getBoolean("bench.compileOnly")) {
  Seq(
    "-Dbench.compileOnly=true"
  )
} else {
  Seq(
    "-Dbench.compileOnly=false"
  )
}

/** Truffle-related settings for test running.
  */
val truffleRunOpts = Seq(
  "-Dpolyglot.compiler.IterativePartialEscape=true",
  "-Dpolyglot.compiler.BackgroundCompilation=false"
)

val truffleRunOptionsSettings = Seq(
  fork := true,
  javaOptions ++= "-ea" +: benchOnlyOptions
)

/** Explicitly provide `application-test.conf` as the resource that should be used for
  * parsing the logging configuration. Explicitly setting `config.resource` prevents
  * the potential conflicts with other *.conf files.
  */
val testLogProviderOptions = Seq(
  "-Dslf4j.provider=org.enso.logger.TestLogProvider",
  "-Dconfig.resource=application-test.conf"
)

/** engine/common project contains classes that are necessary to configure
  * GraalVM's polyglot context. Most specifically it contains `ContextFactory`.
  * As such it needs to depend on `org.graalvm.polyglot` package. Otherwise
  * its dependencies shall be limited - no JSON & co. please. For purposes
  * of consistently setting up loaders, the module depends on `logging-utils`.
  */
lazy val `engine-common` = project
  .in(file("engine/common"))
  .settings(
    frgaalJavaCompilerSetting,
    Test / fork := true,
    commands += WithDebugCommand.withDebug,
    Test / envVars ++= distributionEnvironmentOverrides,
    libraryDependencies ++= Seq(
      "org.graalvm.polyglot" % "polyglot" % graalMavenPackagesVersion % "provided"
    )
  )
  .dependsOn(`logging-config`)
  .dependsOn(`logging-utils`)
  .dependsOn(testkit % Test)

lazy val `polyglot-api` = project
  .in(file("engine/polyglot-api"))
  .settings(
    frgaalJavaCompilerSetting,
    Test / fork := true,
    commands += WithDebugCommand.withDebug,
    Test / envVars ++= distributionEnvironmentOverrides,
    Test / javaOptions ++= Seq(
      "-Dpolyglot.engine.WarnInterpreterOnly=false",
      "-Dpolyglotimpl.DisableClassPathIsolation=true"
    ),
    // Append enso language on the class-path
    Test / unmanagedClasspath :=
      (LocalProject(
        "runtime-fat-jar"
      ) / Compile / fullClasspath).value,
    libraryDependencies ++= Seq(
      "io.circe"                              %% "circe-core"            % circeVersion              % "provided",
      "org.graalvm.sdk"                        % "polyglot-tck"          % graalMavenPackagesVersion % "provided",
      "org.graalvm.truffle"                    % "truffle-api"           % graalMavenPackagesVersion % "provided",
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % jsoniterVersion,
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core"   % jsoniterVersion,
      "com.google.flatbuffers"                 % "flatbuffers-java"      % flatbuffersVersion,
      "org.scalatest"                         %% "scalatest"             % scalatestVersion          % Test,
      "org.scalacheck"                        %% "scalacheck"            % scalacheckVersion         % Test
    ),
    GenerateFlatbuffers.flatcVersion := flatbuffersVersion,
    Compile / sourceGenerators += GenerateFlatbuffers.task
  )
  .dependsOn(`engine-common`)
  .dependsOn(pkg)
  .dependsOn(`text-buffer`)
  .dependsOn(`logging-utils`)
  .dependsOn(testkit % Test)
  .dependsOn(`polyglot-api-macros`)

lazy val `polyglot-api-macros` = project
  .in(file("engine/polyglot-api-macros"))
  .settings(
    frgaalJavaCompilerSetting,
    libraryDependencies ++= Seq(
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core"   % jsoniterVersion % "provided",
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % jsoniterVersion % "provided"
    )
  )

lazy val `language-server` = (project in file("engine/language-server"))
  .enablePlugins(JPMSPlugin)
  .settings(
    commands += WithDebugCommand.withDebug,
    frgaalJavaCompilerSetting,
    libraryDependencies ++= akka ++ circe ++ Seq(
      "org.slf4j"                   % "slf4j-api"               % slf4jVersion,
      "com.typesafe.scala-logging" %% "scala-logging"           % scalaLoggingVersion,
      "io.circe"                   %% "circe-generic-extras"    % circeGenericExtrasVersion,
      "io.circe"                   %% "circe-literal"           % circeVersion,
      "dev.zio"                    %% "zio"                     % zioVersion,
      "com.google.flatbuffers"      % "flatbuffers-java"        % flatbuffersVersion,
      "commons-io"                  % "commons-io"              % commonsIoVersion,
      "com.github.pureconfig"      %% "pureconfig"              % pureconfigVersion,
      akkaTestkit                   % Test,
      "com.typesafe.akka"          %% "akka-http-testkit"       % akkaHTTPVersion           % Test,
      "org.scalatest"              %% "scalatest"               % scalatestVersion          % Test,
      "org.scalacheck"             %% "scalacheck"              % scalacheckVersion         % Test,
      "org.graalvm.truffle"         % "truffle-api"             % graalMavenPackagesVersion % "provided",
      "org.graalvm.sdk"             % "polyglot-tck"            % graalMavenPackagesVersion % "provided",
      "org.netbeans.api"            % "org-openide-util-lookup" % netbeansApiVersion        % "provided",
      "org.eclipse.jgit"            % "org.eclipse.jgit"        % jgitVersion,
      "org.bouncycastle"            % "bcutil-jdk18on"          % "1.76"                    % Test,
      "org.bouncycastle"            % "bcpkix-jdk18on"          % "1.76"                    % Test,
      "org.bouncycastle"            % "bcprov-jdk18on"          % "1.76"                    % Test,
      "org.apache.tika"             % "tika-core"               % tikaVersion               % Test
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
    Test / fork := true,
    // These dependencies are here so that we can use them in `--module-path` later on.
    libraryDependencies ++= {
      val necessaryModules =
        GraalVM.modules.map(_.withConfigurations(Some(Test.name))) ++
        GraalVM.langsPkgs.map(_.withConfigurations(Some(Test.name)))
      necessaryModules
    },
    Test / addModules := Seq(
      (`runtime-fat-jar` / javaModuleName).value,
      (`syntax-rust-definition` / javaModuleName).value,
      (`profiling-utils` / javaModuleName).value,
      (`ydoc-server` / javaModuleName).value
    ),
    Test / moduleDependencies := {
      GraalVM.modules ++ GraalVM.langsPkgs ++ logbackPkg ++ helidon ++ Seq(
        "org.slf4j"        % "slf4j-api"                    % slf4jVersion,
        "org.netbeans.api" % "org-netbeans-modules-sampler" % netbeansApiVersion,
        (`runtime-fat-jar` / projectID).value,
        (`syntax-rust-definition` / projectID).value,
        (`ydoc-server` / projectID).value,
        (`profiling-utils` / projectID).value
      )
    },
    Test / javaOptions ++= testLogProviderOptions,
    Test / patchModules := {

      /** All these modules will be in --patch-module cmdline option to java, which means that
        * for the JVM, it will appear that all the classes contained in these sbt projects are contained
        * in the `org.enso.runtime` module. In this way, we do not have to assembly the `runtime.jar`
        * fat jar.
        */
      val modulesToPatchIntoRuntime: Seq[File] =
        (LocalProject(
          "runtime-instrument-common"
        ) / Compile / productDirectories).value ++
        (LocalProject(
          "runtime-instrument-id-execution"
        ) / Compile / productDirectories).value ++
        (LocalProject(
          "runtime-instrument-repl-debugger"
        ) / Compile / productDirectories).value ++
        (LocalProject(
          "runtime-instrument-runtime-server"
        ) / Compile / productDirectories).value ++
        (LocalProject(
          "runtime-language-epb"
        ) / Compile / productDirectories).value ++
        (LocalProject(
          "runtime-compiler"
        ) / Compile / productDirectories).value ++
        (LocalProject("runtime-parser") / Compile / productDirectories).value ++
        (LocalProject("persistance") / Compile / productDirectories).value ++
        (LocalProject(
          "interpreter-dsl"
        ) / Compile / productDirectories).value ++
        // We have to patch the `runtime` project as well, as it contains BuiltinTypes.metadata in
        // runtime/target/classes/META-INF directory
        (LocalProject("runtime") / Compile / productDirectories).value
      val extraModsToPatch = JPMSUtils.filterModulesFromUpdate(
        (Test / update).value,
        Seq(
          "org.apache.tika" % "tika-core" % tikaVersion
        ),
        streams.value.log,
        shouldContainAll = true
      )
      Map(
        (`runtime-fat-jar` / javaModuleName).value -> (modulesToPatchIntoRuntime ++ extraModsToPatch)
      )
    },
    Test / addReads := {
      Map(
        (`runtime-fat-jar` / javaModuleName).value -> Seq("ALL-UNNAMED")
      )
    },
    Test / addExports := {
      val profModName = (`profiling-utils` / javaModuleName).value
      Map(
        profModName + "/org.enso.profiling.snapshot" -> Seq("ALL-UNNAMED")
      )
    }
  )
  .settings(
    Test / compile := (Test / compile)
      .dependsOn(`runtime-fat-jar` / Compile / compileModuleInfo)
      .value,
    Test / envVars ++= Map(
      "ENSO_EDITION_PATH" -> file("distribution/editions").getCanonicalPath
    )
  )
  .dependsOn(`json-rpc-server-test` % Test)
  .dependsOn(`json-rpc-server`)
  .dependsOn(`task-progress-notifications`)
  .dependsOn(`library-manager`)
  .dependsOn(`connected-lock-manager-server`)
  .dependsOn(`edition-updater`)
  .dependsOn(`engine-runner-common`)
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
  .dependsOn(`runtime-fat-jar` % Test)
  .dependsOn(`library-manager-test` % Test)
  .dependsOn(`runtime-version-manager-test` % Test)
  .dependsOn(`ydoc-server`)

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

/** Common settings for projects whose sources are processed by some annotation
  * processors. These settings ensure that the generated sources are placed under
  * `(Compile/sourceManaged)` directory, usually pointing to `target/classes/src_managed`.
  */
lazy val annotationProcSetting = Seq(
  Compile / compile / javacOptions ++= Seq(
    "-s",
    (Compile / compile / sourceManaged).value.getAbsolutePath,
    "-Xlint:unchecked"
  ),
  Compile / compile := (Compile / compile)
    .dependsOn(Def.task { (Compile / sourceManaged).value.mkdirs })
    .value,
  // zinc cannot see who is generating the java files so it adds some
  // spurious warning messages. The following setting filters out such
  // spurious warnings.
  // See https://stackoverflow.com/questions/55558849/how-do-i-build-a-mixed-java-scala-project-which-uses-java-annotation-code-genera
  Compile / logManager :=
    sbt.internal.util.CustomLogManager.excludeMsg(
      "Could not determine source for class ",
      Level.Warn
    )
)

lazy val javadocSettings = Seq(
  Compile / doc / javacOptions --= Seq(
    "-deprecation",
    "-g",
    "-Xlint:unchecked",
    "-proc:full"
  ),
  Compile / doc / javacOptions ++= Seq(
    "--snippet-path",
    (Test / javaSource).value.getAbsolutePath
  )
)

/** A setting to replace javac with Frgaal compiler, allowing to use latest Java features in the code
  * and still compile down to JDK 17
  */
lazy val frgaalJavaCompilerSetting =
  customFrgaalJavaCompilerSettings(targetJavaVersion)

def customFrgaalJavaCompilerSettings(targetJdk: String) = Seq(
  Compile / compile / compilers := FrgaalJavaCompiler.compilers(
    (Compile / dependencyClasspath).value,
    compilers.value,
    targetJdk
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

lazy val instrumentationSettings =
  frgaalJavaCompilerSetting ++ annotationProcSetting ++ Seq(
    version := ensoVersion,
    commands += WithDebugCommand.withDebug,
    Compile / javacOptions --= Seq(
      "-source",
      frgaalSourceLevel,
      "--enable-preview"
    ),
    libraryDependencies ++= Seq(
      "org.graalvm.truffle" % "truffle-api"           % graalMavenPackagesVersion % "provided",
      "org.graalvm.truffle" % "truffle-dsl-processor" % graalMavenPackagesVersion % "provided"
    )
  )

lazy val `runtime-language-epb` =
  (project in file("engine/runtime-language-epb"))
    .settings(
      frgaalJavaCompilerSetting,
      inConfig(Compile)(truffleRunOptionsSettings),
      truffleDslSuppressWarnsSetting,
      commands += WithDebugCommand.withDebug,
      fork := true,
      Test / javaOptions ++= Seq(),
      instrumentationSettings,
      libraryDependencies ++= Seq(
        "junit"               % "junit"                 % junitVersion              % Test,
        "com.github.sbt"      % "junit-interface"       % junitIfVersion            % Test,
        "org.graalvm.truffle" % "truffle-api"           % graalMavenPackagesVersion % "provided",
        "org.graalvm.truffle" % "truffle-dsl-processor" % graalMavenPackagesVersion % "provided"
      )
    )

lazy val `runtime-language-arrow` =
  (project in file("engine/runtime-language-arrow"))
    .enablePlugins(JPMSPlugin)
    .settings(
      crossPaths := false,
      autoScalaLibrary := false,
      inConfig(Compile)(truffleRunOptionsSettings),
      instrumentationSettings,
      libraryDependencies ++= GraalVM.modules ++ Seq(
        "junit"            % "junit"              % junitVersion       % Test,
        "com.github.sbt"   % "junit-interface"    % junitIfVersion     % Test,
        "org.slf4j"        % "slf4j-nop"          % slf4jVersion       % Test,
        "org.slf4j"        % "slf4j-api"          % slf4jVersion       % Test,
        "org.apache.arrow" % "arrow-vector"       % apacheArrowVersion % Test,
        "org.apache.arrow" % "arrow-memory-netty" % apacheArrowVersion % Test
      ),
      javaModuleName := "org.enso.interpreter.arrow",
      moduleDependencies := GraalVM.modules,
      Test / moduleDependencies +=
        (LocalProject("runtime-language-arrow") / projectID).value,
      Test / patchModules := {
        val testClassesDir = (Test / productDirectories).value.head
        Map(javaModuleName.value -> Seq(testClassesDir))
      },
      Test / addModules := Seq(javaModuleName.value),
      Test / javaOptions ++= Seq(
        s"--add-opens=java.base/java.nio=${javaModuleName.value}", // DirectByteBuffer in MemoryUtil init is in-accessible
        "--add-opens=java.base/java.nio=ALL-UNNAMED" // Tests use Apache Arrow
      ),
      Test / addReads := {
        Map(javaModuleName.value -> Seq("ALL-UNNAMED"))
      }
    )

/** `runtime-test-instruments` project contains Truffle instruments that are used solely for testing.
  * It is compiled into an explicit Java module. Note that this project cannot have compile-time dependency on `runtime`
  * project, so if you need access to classes from `runtime`, you need to use reflection.
  */
lazy val `runtime-test-instruments` =
  (project in file("engine/runtime-test-instruments"))
    .enablePlugins(JPMSPlugin)
    .settings(
      inConfig(Compile)(truffleRunOptionsSettings),
      truffleDslSuppressWarnsSetting,
      instrumentationSettings,
      libraryDependencies ++= GraalVM.modules,
      libraryDependencies ++= Seq(
        "org.graalvm.sdk"     % "polyglot-tck"            % graalMavenPackagesVersion,
        "org.graalvm.truffle" % "truffle-tck"             % graalMavenPackagesVersion,
        "org.graalvm.truffle" % "truffle-tck-common"      % graalMavenPackagesVersion,
        "org.graalvm.truffle" % "truffle-tck-tests"       % graalMavenPackagesVersion,
        "org.netbeans.api"    % "org-openide-util-lookup" % netbeansApiVersion % "provided"
      ),
      javaModuleName := "org.enso.runtime.test",
      moduleDependencies := {
        GraalVM.modules ++ Seq(
          "org.graalvm.sdk"     % "polyglot-tck"            % graalMavenPackagesVersion,
          "org.graalvm.truffle" % "truffle-tck"             % graalMavenPackagesVersion,
          "org.graalvm.truffle" % "truffle-tck-common"      % graalMavenPackagesVersion,
          "org.graalvm.truffle" % "truffle-tck-tests"       % graalMavenPackagesVersion,
          "org.netbeans.api"    % "org-openide-util-lookup" % netbeansApiVersion % "provided"
        )
      }
    )

lazy val runtime = (project in file("engine/runtime"))
  .enablePlugins(JPMSPlugin)
  .settings(
    frgaalJavaCompilerSetting,
    annotationProcSetting,
    truffleDslSuppressWarnsSetting,
    version := ensoVersion,
    commands += WithDebugCommand.withDebug,
    inConfig(Compile)(truffleRunOptionsSettings),
    libraryDependencies ++= GraalVM.langsPkgs ++ Seq(
      "org.apache.commons"   % "commons-lang3"           % commonsLangVersion,
      "org.apache.tika"      % "tika-core"               % tikaVersion,
      "com.lihaoyi"         %% "fansi"                   % fansiVersion,
      "org.graalvm.polyglot" % "polyglot"                % graalMavenPackagesVersion % "provided",
      "org.graalvm.sdk"      % "polyglot-tck"            % graalMavenPackagesVersion % "provided",
      "org.graalvm.truffle"  % "truffle-api"             % graalMavenPackagesVersion % "provided",
      "org.graalvm.truffle"  % "truffle-dsl-processor"   % graalMavenPackagesVersion % "provided",
      "org.netbeans.api"     % "org-openide-util-lookup" % netbeansApiVersion        % "provided",
      "org.scalacheck"      %% "scalacheck"              % scalacheckVersion         % Test,
      "org.scalactic"       %% "scalactic"               % scalacticVersion          % Test,
      "org.scalatest"       %% "scalatest"               % scalatestVersion          % Test,
      "junit"                % "junit"                   % junitVersion              % Test,
      "com.github.sbt"       % "junit-interface"         % junitIfVersion            % Test,
      "org.hamcrest"         % "hamcrest-all"            % hamcrestVersion           % Test,
      "org.slf4j"            % "slf4j-api"               % slf4jVersion              % Test
    ),
    // Add all GraalVM packages with Runtime scope - we don't need them for compilation,
    // just provide them at runtime (in module-path).
    libraryDependencies ++= {
      val necessaryModules =
        GraalVM.modules.map(_.withConfigurations(Some(Runtime.name)))
      val langs =
        GraalVM.langsPkgs.map(_.withConfigurations(Some(Runtime.name)))
      val tools =
        GraalVM.toolsPkgs.map(_.withConfigurations(Some(Runtime.name)))
      necessaryModules ++ langs ++ tools
    }
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
      .dependsOn(`std-snowflake` / Compile / packageBin)
      .dependsOn(`std-microsoft` / Compile / packageBin)
      .dependsOn(`std-tableau` / Compile / packageBin)
      .value
  )
  .dependsOn(`common-polyglot-core-utils`)
  .dependsOn(`edition-updater`)
  .dependsOn(`interpreter-dsl` % "provided")
  .dependsOn(`persistance-dsl` % "provided")
  .dependsOn(`library-manager`)
  .dependsOn(`logging-truffle-connector`)
  .dependsOn(`polyglot-api`)
  .dependsOn(`text-buffer`)
  .dependsOn(`runtime-compiler`)
  .dependsOn(`runtime-suggestions`)
  .dependsOn(`connected-lock-manager`)
  .dependsOn(testkit % Test)

/** A project holding all the runtime integration tests. These tests require, among other things,
  * the `org.enso.runtime` JPMS module, so it is easier to keep them in a separate project.
  * For standard unit tests, use `runtime/Test`.
  */
lazy val `runtime-integration-tests` =
  (project in file("engine/runtime-integration-tests"))
    .enablePlugins(JPMSPlugin)
    .settings(
      frgaalJavaCompilerSetting,
      annotationProcSetting,
      commands += WithDebugCommand.withDebug,
      libraryDependencies ++= GraalVM.modules ++ GraalVM.langsPkgs ++ GraalVM.insightPkgs ++ logbackPkg ++ helidon ++ Seq(
        "org.graalvm.polyglot" % "polyglot"              % graalMavenPackagesVersion % "provided",
        "org.graalvm.sdk"      % "polyglot-tck"          % graalMavenPackagesVersion % "provided",
        "org.graalvm.truffle"  % "truffle-api"           % graalMavenPackagesVersion % "provided",
        "org.graalvm.truffle"  % "truffle-dsl-processor" % graalMavenPackagesVersion % "provided",
        "org.graalvm.truffle"  % "truffle-tck"           % graalMavenPackagesVersion % Test,
        "org.graalvm.truffle"  % "truffle-tck-common"    % graalMavenPackagesVersion % Test,
        "org.graalvm.truffle"  % "truffle-tck-tests"     % graalMavenPackagesVersion % Test,
        "org.scalacheck"      %% "scalacheck"            % scalacheckVersion         % Test,
        "org.scalactic"       %% "scalactic"             % scalacticVersion          % Test,
        "org.scalatest"       %% "scalatest"             % scalatestVersion          % Test,
        "junit"                % "junit"                 % junitVersion              % Test,
        "com.github.sbt"       % "junit-interface"       % junitIfVersion            % Test,
        "org.hamcrest"         % "hamcrest-all"          % hamcrestVersion           % Test,
        "org.slf4j"            % "slf4j-api"             % slf4jVersion              % Test
      ),
      Test / compile := (Test / compile)
        .dependsOn(`runtime-fat-jar` / Compile / compileModuleInfo)
        .value,
      Test / fork := true,
      Test / parallelExecution := false,
      Test / logBuffered := false,
      Test / envVars ++= distributionEnvironmentOverrides ++ Map(
        "ENSO_TEST_DISABLE_IR_CACHE" -> "false",
        "ENSO_EDITION_PATH"          -> file("distribution/editions").getCanonicalPath
      ),
      inConfig(Test)(truffleRunOptionsSettings),
      Test / javaOptions ++= Seq(
        "-Dtck.values=java-host,enso",
        "-Dtck.language=enso",
        "-Dtck.inlineVerifierInstrument=false",
        "-Dpolyglot.engine.AllowExperimentalOptions=true"
      ),
      Test / javaOptions ++= testLogProviderOptions,
      Test / addModules := Seq(
        (`runtime-test-instruments` / javaModuleName).value,
        (`runtime-fat-jar` / javaModuleName).value,
        (`syntax-rust-definition` / javaModuleName).value,
        (`profiling-utils` / javaModuleName).value,
        (`ydoc-server` / javaModuleName).value
      ),
      Test / moduleDependencies := {
        GraalVM.modules ++ GraalVM.langsPkgs ++ GraalVM.insightPkgs ++ logbackPkg ++ helidon ++ Seq(
          "org.slf4j"           % "slf4j-api"                    % slf4jVersion,
          "org.netbeans.api"    % "org-openide-util-lookup"      % netbeansApiVersion,
          "org.netbeans.api"    % "org-netbeans-modules-sampler" % netbeansApiVersion,
          "org.graalvm.sdk"     % "polyglot-tck"                 % graalMavenPackagesVersion,
          "org.graalvm.truffle" % "truffle-tck"                  % graalMavenPackagesVersion,
          "org.graalvm.truffle" % "truffle-tck-common"           % graalMavenPackagesVersion,
          "org.graalvm.truffle" % "truffle-tck-tests"            % graalMavenPackagesVersion,
          (`runtime-test-instruments` / projectID).value,
          (`runtime-fat-jar` / projectID).value,
          (`ydoc-server` / projectID).value,
          (`syntax-rust-definition` / projectID).value,
          (`profiling-utils` / projectID).value
        )
      },
      Test / patchModules := {

        /** All these modules will be in --patch-module cmdline option to java, which means that
          * for the JVM, it will appear that all the classes contained in these sbt projects are contained
          * in the `org.enso.runtime` module. In this way, we do not have to assembly the `runtime.jar`
          * fat jar.
          */
        val modulesToPatchIntoRuntime: Seq[File] =
          (LocalProject(
            "runtime-instrument-common"
          ) / Compile / productDirectories).value ++
          (LocalProject(
            "runtime-instrument-id-execution"
          ) / Compile / productDirectories).value ++
          (LocalProject(
            "runtime-instrument-repl-debugger"
          ) / Compile / productDirectories).value ++
          (LocalProject(
            "runtime-instrument-runtime-server"
          ) / Compile / productDirectories).value ++
          (LocalProject(
            "runtime-language-epb"
          ) / Compile / productDirectories).value ++
          (LocalProject(
            "runtime-compiler"
          ) / Compile / productDirectories).value ++
          (LocalProject(
            "refactoring-utils"
          ) / Compile / productDirectories).value ++
          (LocalProject(
            "runtime-instrument-common"
          ) / Test / productDirectories).value
        // Patch test-classes into the runtime module. This is standard way to deal with the
        // split package problem in unit tests. For example, Maven's surefire plugin does this.
        val testClassesDir = (Test / productDirectories).value.head
        Map(
          (`runtime-fat-jar` / javaModuleName).value -> (modulesToPatchIntoRuntime ++ Seq(
            testClassesDir
          ))
        )
      },
      Test / addReads := {
        val runtimeModName = (`runtime-fat-jar` / javaModuleName).value
        val testInstrumentsModName =
          (`runtime-test-instruments` / javaModuleName).value
        Map(
          // We patched the test-classes into the runtime module. These classes access some stuff from
          // unnamed module. Thus, let's add ALL-UNNAMED.
          runtimeModName -> Seq(
            "ALL-UNNAMED",
            testInstrumentsModName,
            "truffle.tck.tests",
            "org.openide.util.lookup.RELEASE180"
          ),
          testInstrumentsModName -> Seq(runtimeModName)
        )
      }
    )
    .dependsOn(`runtime-fat-jar`)
    .dependsOn(`runtime-test-instruments`)
    .dependsOn(`logging-service-logback` % "test->test")
    .dependsOn(testkit % Test)
    .dependsOn(`connected-lock-manager-server`)
    .dependsOn(`test-utils`)

/** A project that holds only benchmarks for `runtime`. Unlike `runtime-integration-tests`, its execution requires
  * the whole `runtime-fat-jar` assembly, as we want to be as close to the enso distribution as possible.
  */
lazy val `runtime-benchmarks` =
  (project in file("engine/runtime-benchmarks"))
    .enablePlugins(JPMSPlugin)
    .settings(
      frgaalJavaCompilerSetting,
      annotationProcSetting,
      // Note that withDebug command only makes sense if you use `@Fork(0)` in your benchmarks.
      commands += WithDebugCommand.withDebug,
      libraryDependencies ++= GraalVM.modules ++ GraalVM.langsPkgs ++ GraalVM.toolsPkgs ++ helidon ++ Seq(
        "org.openjdk.jmh"     % "jmh-core"                 % jmhVersion,
        "org.openjdk.jmh"     % "jmh-generator-annprocess" % jmhVersion,
        "jakarta.xml.bind"    % "jakarta.xml.bind-api"     % jaxbVersion,
        "com.sun.xml.bind"    % "jaxb-impl"                % jaxbVersion,
        "org.graalvm.truffle" % "truffle-api"              % graalMavenPackagesVersion,
        "org.graalvm.truffle" % "truffle-dsl-processor"    % graalMavenPackagesVersion % "provided",
        "org.slf4j"           % "slf4j-api"                % slf4jVersion,
        "org.slf4j"           % "slf4j-nop"                % slf4jVersion
      ),
      mainClass :=
        Some("org.enso.interpreter.bench.benchmarks.RuntimeBenchmarksRunner"),
      javacOptions --= Seq(
        "-source",
        frgaalSourceLevel,
        "--enable-preview"
      ),
      Compile / compile := (Compile / compile)
        .dependsOn(`runtime-fat-jar` / assembly)
        .value,
      parallelExecution := false,
      moduleDependencies := {
        GraalVM.modules ++ GraalVM.langsPkgs ++ helidon ++ Seq(
          "org.slf4j"        % "slf4j-api"                    % slf4jVersion,
          "org.slf4j"        % "slf4j-nop"                    % slf4jVersion,
          "org.netbeans.api" % "org-netbeans-modules-sampler" % netbeansApiVersion,
          (`ydoc-server` / projectID).value,
          (`syntax-rust-definition` / projectID).value,
          (`profiling-utils` / projectID).value
        )
      },
      modulePath += {
        (`runtime-fat-jar` / assembly / assemblyOutputPath).value
      },
      addModules := {
        val runtimeModuleName = (`runtime-fat-jar` / javaModuleName).value
        Seq(runtimeModuleName)
      },
      addExports := {
        Map("org.slf4j.nop/org.slf4j.nop" -> Seq("org.slf4j"))
      },
      javaOptions ++= {
        Seq(
          // To enable logging in benchmarks, add ch.qos.logback module on the modulePath
          "-Dslf4j.provider=org.slf4j.nop.NOPServiceProvider"
        )
      },
      javaOptions ++= benchOnlyOptions,
      javaOptions += "-Xss16M",
      run / fork := true,
      run / connectInput := true,
      bench := Def
        .task {
          (Compile / run).toTask("").tag(Exclusive).value
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
          (Compile / run).toTask(" " + name).value
        }
      }.evaluated
    )
    .dependsOn(`runtime-fat-jar`)
    .dependsOn(`benchmarks-common`)
    .dependsOn(`test-utils`)

lazy val `runtime-parser` =
  (project in file("engine/runtime-parser"))
    .settings(
      version := mavenUploadVersion,
      javadocSettings,
      publish / skip := false,
      crossPaths := false,
      frgaalJavaCompilerSetting,
      annotationProcSetting,
      commands += WithDebugCommand.withDebug,
      fork := true,
      libraryDependencies ++= Seq(
        "junit"            % "junit"                   % junitVersion       % Test,
        "com.github.sbt"   % "junit-interface"         % junitIfVersion     % Test,
        "org.scalatest"   %% "scalatest"               % scalatestVersion   % Test,
        "org.netbeans.api" % "org-openide-util-lookup" % netbeansApiVersion % "provided"
      )
    )
    .dependsOn(`syntax-rust-definition`)
    .dependsOn(`persistance`)
    .dependsOn(`persistance-dsl` % "provided")

lazy val `runtime-compiler` =
  (project in file("engine/runtime-compiler"))
    .settings(
      frgaalJavaCompilerSetting,
      annotationProcSetting,
      (Test / fork) := true,
      libraryDependencies ++= Seq(
        "junit"            % "junit"                   % junitVersion       % Test,
        "com.github.sbt"   % "junit-interface"         % junitIfVersion     % Test,
        "org.scalatest"   %% "scalatest"               % scalatestVersion   % Test,
        "org.netbeans.api" % "org-openide-util-lookup" % netbeansApiVersion % "provided"
      )
    )
    .dependsOn(`runtime-parser`)
    .dependsOn(pkg)
    .dependsOn(`engine-common`)
    .dependsOn(editions)
    .dependsOn(`persistance-dsl` % "provided")

lazy val `runtime-suggestions` =
  (project in file("engine/runtime-suggestions"))
    .settings(
      frgaalJavaCompilerSetting,
      (Test / fork) := true,
      libraryDependencies ++= Seq(
        "junit"            % "junit"                   % junitVersion       % Test,
        "com.github.sbt"   % "junit-interface"         % junitIfVersion     % Test,
        "org.scalatest"   %% "scalatest"               % scalatestVersion   % Test,
        "org.netbeans.api" % "org-openide-util-lookup" % netbeansApiVersion % "provided"
      )
    )
    .dependsOn(`runtime-compiler`)
    .dependsOn(`polyglot-api`)

lazy val `runtime-instrument-common` =
  (project in file("engine/runtime-instrument-common"))
    .configs(Benchmark)
    .settings(
      frgaalJavaCompilerSetting,
      inConfig(Compile)(truffleRunOptionsSettings),
      inConfig(Benchmark)(Defaults.testSettings),
      instrumentationSettings,
      Test / javaOptions ++= Seq(
        "-Dpolyglotimpl.DisableClassPathIsolation=true"
      ),
      bench := (Benchmark / test).tag(Exclusive).value,
      Benchmark / parallelExecution := false,
      (Benchmark / javaOptions) :=
        (LocalProject("std-benchmarks") / Compile / javaOptions).value,
      Test / fork := true,
      Test / envVars ++= distributionEnvironmentOverrides ++ Map(
        "ENSO_TEST_DISABLE_IR_CACHE" -> "false"
      ),
      libraryDependencies ++= Seq(
        "junit"          % "junit"           % junitVersion     % Test,
        "com.github.sbt" % "junit-interface" % junitIfVersion   % Test,
        "org.scalatest" %% "scalatest"       % scalatestVersion % Test
      )
    )
    .dependsOn(`refactoring-utils`)
    .dependsOn(
      LocalProject(
        "runtime"
      ) % "compile->compile;runtime->runtime;bench->bench"
    )

lazy val `runtime-instrument-id-execution` =
  (project in file("engine/runtime-instrument-id-execution"))
    .settings(
      frgaalJavaCompilerSetting,
      inConfig(Compile)(truffleRunOptionsSettings),
      instrumentationSettings
    )
    .dependsOn(LocalProject("runtime"))
    .dependsOn(`runtime-instrument-common`)

lazy val `runtime-instrument-repl-debugger` =
  (project in file("engine/runtime-instrument-repl-debugger"))
    .settings(
      inConfig(Compile)(truffleRunOptionsSettings),
      instrumentationSettings
    )
    .dependsOn(LocalProject("runtime"))
    .dependsOn(`runtime-instrument-common`)

lazy val `runtime-instrument-runtime-server` =
  (project in file("engine/runtime-instrument-runtime-server"))
    .settings(
      inConfig(Compile)(truffleRunOptionsSettings),
      instrumentationSettings
    )
    .dependsOn(LocalProject("runtime"))
    .dependsOn(`runtime-instrument-common` % "test->test;compile->compile")

/** A "meta" project that exists solely to provide logic for assembling the `runtime.jar` fat Jar.
  * We do not want to put this task into any other existing project, as it internally copies some
  * classes from other projects into the `classes` directory, therefore, pollutes the build.
  * There is only one Java source in this project - `module-info.java`. During the assembling of the
  * fat jar, all the classes from the dependent projects are copied into the `classes` directory of
  * this project and then, a custom task is invoked to compile the `module-info.java`.
  */
lazy val `runtime-fat-jar` =
  (project in file("engine/runtime-fat-jar"))
    .enablePlugins(JPMSPlugin)
    .settings(
      // extra module path for compileModuleInfo task
      Compile / JPMSUtils.extraMp := {
        val ydocMod =
          (`ydoc-server` / Compile / exportedProductJars).value
        val syntaxMod =
          (`syntax-rust-definition` / Compile / exportedProductJars).value
        val profilingMod =
          (`profiling-utils` / Compile / exportedProductJars).value
        ydocMod ++ syntaxMod ++ profilingMod
      },
      Compile / compileModuleInfo := {
        JPMSUtils.compileModuleInfo(
          copyDepsFilter = ScopeFilter(
            inProjects(
              LocalProject("runtime"),
              LocalProject("runtime-language-epb"),
              LocalProject("runtime-instrument-common"),
              LocalProject("runtime-instrument-id-execution"),
              LocalProject("runtime-instrument-repl-debugger"),
              LocalProject("runtime-instrument-runtime-server")
            ),
            inConfigurations(Compile)
          ),
          modulePath = JPMSUtils.componentModules ++ helidon
        )
      }
        .dependsOn(Compile / compile)
        .value,
      // Filter module-info.java from the compilation
      excludeFilter := excludeFilter.value || "module-info.java",
      javaModuleName := "org.enso.runtime",
      compileOrder := CompileOrder.JavaThenScala
    )
    /** The following libraryDependencies are provided in Runtime scope.
      * Later, we will collect them into --module-path option.
      * We don't collect them in Compile scope as it does not even make sense
      * to run `compile` task in this project.
      */
    .settings(
      libraryDependencies ++= {
        val graalMods =
          GraalVM.modules.map(_.withConfigurations(Some(Runtime.name)))
        val langMods =
          GraalVM.langsPkgs.map(_.withConfigurations(Some(Runtime.name)))
        val logbackMods =
          logbackPkg.map(_.withConfigurations(Some(Runtime.name)))
        val helidonMods = helidon.map(_.withConfigurations(Some(Runtime.name)))
        graalMods ++ langMods ++ logbackMods ++ helidonMods
      }
    )
    /** Assembling Uber Jar */
    .settings(
      assembly := assembly
        .dependsOn(Compile / compile)
        .dependsOn(Compile / compileModuleInfo)
        //`pakcageBin`dependencies are needed because
        // assemblyExcludedJars can only exclude JAR archives, not exploded
        // directories with classes.
        .dependsOn(`syntax-rust-definition` / Compile / packageBin)
        .dependsOn(`ydoc-server` / Compile / packageBin)
        .dependsOn(`profiling-utils` / Compile / packageBin)
        .value,
      assembly / assemblyJarName := "runtime.jar",
      assembly / test := {},
      assembly / assemblyOutputPath := file("runtime.jar"),
      assembly / assemblyExcludedJars := {
        val pkgsToExclude = JPMSUtils.componentModules ++ helidon ++ Seq(
          "org.netbeans.api" % "org-netbeans-modules-sampler" % netbeansApiVersion
        )
        val ourFullCp = (Runtime / fullClasspath).value
        val excludedExternalPkgs = JPMSUtils.filterModulesFromClasspath(
          ourFullCp,
          pkgsToExclude,
          streams.value.log
        )
        val syntaxJar =
          (`syntax-rust-definition` / Compile / exportedProducts).value
        val ydocJar = (`ydoc-server` / Compile / exportedProducts).value
        val profilingJar =
          (`profiling-utils` / Compile / exportedProducts).value
        val excludedInternalPkgs = syntaxJar ++ ydocJar ++ profilingJar
        val log                  = streams.value.log
        excludedInternalPkgs.foreach { internalPkg =>
          val isJar =
            internalPkg.data.exists() && internalPkg.data.name.endsWith(".jar")
          if (!isJar) {
            log.error(
              internalPkg.data.absolutePath + " is not a JAR archive." +
              " It might not be excluded from runtime.jar fat jar."
            )
          }
        }
        excludedExternalPkgs ++ excludedInternalPkgs
      },
      assembly / assemblyMergeStrategy := {
        case PathList("META-INF", file, xs @ _*) if file.endsWith(".DSA") =>
          MergeStrategy.discard
        case PathList("META-INF", file, xs @ _*) if file.endsWith(".SF") =>
          MergeStrategy.discard
        case PathList("META-INF", "MANIFEST.MF", xs @ _*) =>
          MergeStrategy.discard
        case PathList("META-INF", "services", xs @ _*) =>
          MergeStrategy.concat
        case PathList("module-info.class") =>
          MergeStrategy.preferProject
        // remove once https://github.com/snakeyaml/snakeyaml/pull/12 gets integrated
        case PathList("org", "yaml", "snakeyaml", "introspector", xs @ _*) =>
          MergeStrategy.preferProject
        case PathList(xs @ _*) if xs.last.contains("module-info.class") =>
          MergeStrategy.discard
        case _ => MergeStrategy.first
      }
    )
    .dependsOn(`runtime-instrument-common`)
    .dependsOn(`runtime-instrument-id-execution`)
    .dependsOn(`runtime-instrument-repl-debugger`)
    .dependsOn(`runtime-instrument-runtime-server`)
    .dependsOn(`runtime-language-epb`)
    .dependsOn(`ydoc-server`)
    .dependsOn(yaml)
    .dependsOn(LocalProject("runtime"))

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

/* The purpose of the `engine-runner-common` project is to contain everything
 * that's needed for the `engine-runner` project to invoke `language-server` when
 * `--server` option is used.
 *
 * As such this project contains (primarily) the `LanguageServerApi`
 * API & SPI class. `engine-runner` project call the `LanguageServerApi` class static method
 * and that method then delegates to an implementation which is supposed to be provided
 * by the `language-server` project.
 *
 * `engine-runner` and `language-server` projects shall be "loosely coupled" - they shouldn't
 * have compile time dependency between each other. All that's needed for them to
 * communicate belongs into `engine-runner-common` project.
 */
lazy val `engine-runner-common` = project
  .in(file("engine/runner-common"))
  .settings(
    frgaalJavaCompilerSetting,
    Test / fork := true,
    commands += WithDebugCommand.withDebug,
    Test / envVars ++= distributionEnvironmentOverrides,
    libraryDependencies ++= Seq(
      "org.graalvm.polyglot" % "polyglot"    % graalMavenPackagesVersion % "provided",
      "commons-io"           % "commons-io"  % commonsIoVersion,
      "commons-cli"          % "commons-cli" % commonsCliVersion
    )
  )
  .dependsOn(`polyglot-api`)
  .dependsOn(`library-manager`)
  .dependsOn(`edition-updater`)
  .dependsOn(testkit % Test)

lazy val `engine-runner` = project
  .in(file("engine/runner"))
  .settings(
    frgaalJavaCompilerSetting,
    truffleDslSuppressWarnsSetting,
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
    assembly / assemblyExcludedJars := {
      val excludedExternalPkgs = JPMSUtils.filterArtifacts(
        (Compile / fullClasspath).value,
        "graalvm",
        "truffle",
        "helidon"
      )
      val syntaxJar =
        (`syntax-rust-definition` / Compile / exportedProducts).value
      val ydocJar              = (`ydoc-server` / Compile / exportedProducts).value
      val profilingJar         = (`profiling-utils` / Compile / exportedProducts).value
      val excludedInternalPkgs = syntaxJar ++ ydocJar ++ profilingJar
      val log                  = streams.value.log
      excludedInternalPkgs.foreach { internalPkg =>
        val isJar =
          internalPkg.data.exists() && internalPkg.data.name.endsWith(".jar")
        if (!isJar) {
          log.error(
            internalPkg.data.absolutePath + " is not a JAR archive." +
            " It might not be excluded from runtime.jar fat jar."
          )
        }
      }
      excludedInternalPkgs ++ excludedExternalPkgs
    },
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
      // remove once https://github.com/snakeyaml/snakeyaml/pull/12 gets integrated
      case PathList("org", "yaml", "snakeyaml", "introspector", xs @ _*) =>
        MergeStrategy.preferProject
      case PathList(xs @ _*) if xs.last.contains("module-info") =>
        // runner.jar must not be a JPMS module
        MergeStrategy.discard
      case x =>
        MergeStrategy.first
    },
    commands += WithDebugCommand.withDebug,
    inConfig(Compile)(truffleRunOptionsSettings),
    libraryDependencies ++= Seq(
      "org.graalvm.polyglot"    % "polyglot"                % graalMavenPackagesVersion,
      "org.graalvm.sdk"         % "polyglot-tck"            % graalMavenPackagesVersion % Provided,
      "commons-cli"             % "commons-cli"             % commonsCliVersion,
      "com.monovore"           %% "decline"                 % declineVersion,
      "org.jline"               % "jline"                   % jlineVersion,
      "junit"                   % "junit"                   % junitVersion              % Test,
      "com.github.sbt"          % "junit-interface"         % junitIfVersion            % Test,
      "org.hamcrest"            % "hamcrest-all"            % hamcrestVersion           % Test,
      "org.scala-lang.modules" %% "scala-collection-compat" % scalaCollectionCompatVersion
    ),
    run / connectInput := true
  )
  .settings(
    NativeImage.smallJdk := Some(buildSmallJdk.value),
    NativeImage.additionalCp := {
      val core = Seq(
        "runtime.jar",
        "runner.jar"
      )
      val stdLibsJars =
        `base-polyglot-root`.listFiles("*.jar").map(_.getAbsolutePath())
      val profJar = (`profiling-utils` / Compile / exportedProductJars).value
        .map(_.data.getAbsolutePath)
      val syntaxJar =
        (`syntax-rust-definition` / Compile / exportedProductJars).value
          .map(_.data.getAbsolutePath)
      core ++ stdLibsJars ++ profJar ++ syntaxJar
    },
    buildSmallJdk := {
      val smallJdkDirectory = (target.value / "jdk").getAbsoluteFile()
      if (smallJdkDirectory.exists()) {
        IO.delete(smallJdkDirectory)
      }
      val NI_MODULES =
        "org.graalvm.nativeimage,org.graalvm.nativeimage.builder,org.graalvm.nativeimage.base,org.graalvm.nativeimage.driver,org.graalvm.nativeimage.librarysupport,org.graalvm.nativeimage.objectfile,org.graalvm.nativeimage.pointsto,com.oracle.graal.graal_enterprise,com.oracle.svm.svm_enterprise"
      val JDK_MODULES =
        "jdk.localedata,jdk.compiler.graal,jdk.httpserver,java.naming,java.net.http"
      val DEBUG_MODULES  = "jdk.jdwp.agent"
      val PYTHON_MODULES = "jdk.security.auth,java.naming"

      val javaHome = Option(System.getProperty("java.home")).map(Paths.get(_))
      val (jlink, modules, libDirs) = javaHome match {
        case None =>
          throw new RuntimeException("Missing java.home variable")
        case Some(jh) =>
          val exec = jh.resolve("bin").resolve("jlink")
          val moduleJars = List(
            "lib/svm/bin/../../graalvm/svm-driver.jar",
            "lib/svm/bin/../builder/native-image-base.jar",
            "lib/svm/bin/../builder/objectfile.jar",
            "lib/svm/bin/../builder/pointsto.jar",
            "lib/svm/bin/../builder/svm-enterprise.jar",
            "lib/svm/bin/../builder/svm.jar",
            "lib/svm/bin/../library-support.jar"
          )
          val targetLibDirs = List("graalvm", "svm", "static", "truffle")
          (
            exec,
            moduleJars.map(jar => jh.resolve(jar).toString),
            targetLibDirs.map(d => jh.resolve("lib").resolve(d))
          )
      }

      var jlinkArgs = Seq(
        "--module-path",
        modules.mkString(File.pathSeparator),
        "--output",
        smallJdkDirectory.toString(),
        "--add-modules",
        s"$NI_MODULES,$JDK_MODULES,$DEBUG_MODULES,$PYTHON_MODULES"
      )
      val exitCode = scala.sys.process.Process(jlink.toString(), jlinkArgs).!
      if (exitCode != 0) {
        throw new RuntimeException(
          s"Failed to execute $jlink ${jlinkArgs.mkString(" ")} - exit code: $exitCode"
        )
      }
      libDirs.foreach(libDir =>
        IO.copyDirectory(
          libDir.toFile,
          smallJdkDirectory.toPath
            .resolve("lib")
            .resolve(libDir.toFile.getName)
            .toFile
        )
      )
      assert(
        smallJdkDirectory.exists(),
        "Directory of small JDK " + smallJdkDirectory + " is not present"
      )
      smallJdkDirectory
    },
    assembly := assembly
      .dependsOn(`runtime-fat-jar` / assembly)
      .value,
    rebuildNativeImage := Def
      .taskDyn {
        NativeImage
          .buildNativeImage(
            "enso",
            targetDir     = engineDistributionRoot.value / "bin",
            staticOnLinux = false,
            additionalOptions = Seq(
              "-Dorg.apache.commons.logging.Log=org.apache.commons.logging.impl.NoOpLog",
              "-H:IncludeResources=.*Main.enso$",
              "-H:+AddAllCharsets",
              "-H:+IncludeAllLocales",
              "-ea",
              // useful perf & debug switches:
              // "-g",
              // "-H:+SourceLevelDebug",
              // "-H:-DeleteLocalSymbols",
              // you may need to set smallJdk := None to use following flags:
              // "--trace-class-initialization=org.enso.syntax2.Parser",
              "-Dnic=nic"
            ),
            mainClass = Some("org.enso.runner.Main"),
            initializeAtRuntime = Seq(
              "org.jline.nativ.JLineLibrary",
              "org.jline.terminal.impl.jna",
              "io.methvin.watchservice.jna.CarbonAPI",
              "zio.internal.ZScheduler$$anon$4",
              "org.enso.runner.Main$",
              "sun.awt",
              "sun.java2d",
              "sun.font",
              "java.awt",
              "com.sun.imageio",
              "com.sun.jna.internal.Cleaner",
              "com.sun.jna.Structure$FFIType",
              "akka.http"
            )
          )
      }
      .dependsOn(NativeImage.additionalCp)
      .dependsOn(NativeImage.smallJdk)
      .dependsOn(assembly)
      .dependsOn(
        buildEngineDistribution
      )
      .value,
    buildNativeImage := Def.taskDyn {
      NativeImage
        .incrementalNativeImageBuild(
          rebuildNativeImage,
          "enso",
          targetDir = engineDistributionRoot.value / "bin"
        )
    }.value
  )
  .dependsOn(`version-output`)
  .dependsOn(yaml)
  .dependsOn(pkg)
  .dependsOn(cli)
  .dependsOn(`profiling-utils`)
  .dependsOn(`library-manager`)
  .dependsOn(`edition-updater`)
  .dependsOn(`runtime-parser`)
  .dependsOn(`logging-service`)
  .dependsOn(`logging-service-logback` % Runtime)
  .dependsOn(`engine-runner-common`)
  .dependsOn(`polyglot-api`)
  .dependsOn(`enso-test-java-helpers`)
  .dependsOn(`language-server` % Runtime)

lazy val buildSmallJdk =
  taskKey[File]("Build a minimal JDK used for native image generation")

lazy val launcher = project
  .in(file("engine/launcher"))
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    resolvers += Resolver.bintrayRepo("gn0s1s", "releases"),
    commands += WithDebugCommand.withDebug,
    libraryDependencies ++= Seq(
      "com.typesafe.scala-logging" %% "scala-logging"    % scalaLoggingVersion,
      "org.apache.commons"          % "commons-compress" % commonsCompressVersion,
      "org.scalatest"              %% "scalatest"        % scalatestVersion % Test,
      akkaSLF4J
    )
  )
  .settings(
    NativeImage.smallJdk := None,
    NativeImage.additionalCp := Seq.empty,
    rebuildNativeImage := NativeImage
      .buildNativeImage(
        "ensoup",
        staticOnLinux = true,
        additionalOptions = Seq(
          "-Dorg.apache.commons.logging.Log=org.apache.commons.logging.impl.NoOpLog",
          "-H:IncludeResources=.*Main.enso$"
        ),
        includeRuntime = false,
        mainClass      = Some("org.enso.launcher.cli.Main")
      )
      .dependsOn(assembly)
      .dependsOn(VerifyReflectionSetup.run)
      .value,
    buildNativeImage := NativeImage
      .incrementalNativeImageBuild(
        rebuildNativeImage,
        "ensoup"
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
      // launcher.jar must not be an explicit Jar module
      case PathList(xs @ _*) if xs.last.contains("module-info") =>
        MergeStrategy.discard
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
    Test / fork := true
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
      "org.yaml"                    % "snakeyaml"     % snakeyamlVersion,
      "commons-io"                  % "commons-io"    % commonsIoVersion,
      "org.scalatest"              %% "scalatest"     % scalatestVersion % Test
    )
  )
  .dependsOn(editions)
  .dependsOn(cli)
  .dependsOn(pkg)
  .dependsOn(`logging-utils`)

lazy val `test-utils` =
  (project in file("lib/java/test-utils"))
    .settings(
      frgaalJavaCompilerSetting,
      annotationProcSetting,
      libraryDependencies ++= GraalVM.modules,
      libraryDependencies ++= Seq(
        "org.graalvm.truffle" % "truffle-api"           % graalMavenPackagesVersion % "provided",
        "org.graalvm.truffle" % "truffle-dsl-processor" % graalMavenPackagesVersion % "provided"
      ),
      Compile / javacOptions ++= Seq(
        "-s",
        (Compile / sourceManaged).value.getAbsolutePath
      ),
      Compile / compile := (Compile / compile)
        .dependsOn(Def.task { (Compile / sourceManaged).value.mkdirs })
        .value
    )
    .dependsOn(runtime)

lazy val `benchmarks-common` =
  (project in file("lib/java/benchmarks-common"))
    .settings(
      frgaalJavaCompilerSetting,
      libraryDependencies ++= GraalVM.modules ++ Seq(
        "org.openjdk.jmh"  % "jmh-core"                 % jmhVersion,
        "org.openjdk.jmh"  % "jmh-generator-annprocess" % jmhVersion,
        "jakarta.xml.bind" % "jakarta.xml.bind-api"     % jaxbVersion,
        "com.sun.xml.bind" % "jaxb-impl"                % jaxbVersion
      )
    )
    .dependsOn(`polyglot-api`)

lazy val `desktop-environment` =
  project
    .in(file("lib/java/desktop-environment"))
    .settings(
      frgaalJavaCompilerSetting,
      libraryDependencies ++= Seq(
        "org.graalvm.sdk" % "graal-sdk"       % graalMavenPackagesVersion % "provided",
        "commons-io"      % "commons-io"      % commonsIoVersion,
        "org.slf4j"       % "slf4j-api"       % slf4jVersion,
        "junit"           % "junit"           % junitVersion              % Test,
        "com.github.sbt"  % "junit-interface" % junitIfVersion            % Test
      )
    )

lazy val `bench-processor` = (project in file("lib/scala/bench-processor"))
  .settings(
    frgaalJavaCompilerSetting,
    libraryDependencies ++= Seq(
      "jakarta.xml.bind"     % "jakarta.xml.bind-api"     % jaxbVersion,
      "com.sun.xml.bind"     % "jaxb-impl"                % jaxbVersion,
      "org.openjdk.jmh"      % "jmh-core"                 % jmhVersion                % "provided",
      "org.openjdk.jmh"      % "jmh-generator-annprocess" % jmhVersion                % "provided",
      "org.netbeans.api"     % "org-openide-util-lookup"  % netbeansApiVersion        % "provided",
      "org.graalvm.polyglot" % "polyglot"                 % graalMavenPackagesVersion % "provided",
      "junit"                % "junit"                    % junitVersion              % Test,
      "com.github.sbt"       % "junit-interface"          % junitIfVersion            % Test,
      "org.graalvm.truffle"  % "truffle-api"              % graalMavenPackagesVersion % Test
    ),
    Compile / javacOptions := ((Compile / javacOptions).value ++
    // Only run ServiceProvider processor and ignore those defined in META-INF, thus
    // fixing incremental compilation setup
    Seq(
      "-processor",
      "org.netbeans.modules.openide.util.ServiceProviderProcessor"
    )),
    mainClass := Some("org.enso.benchmarks.libs.LibBenchRunner"),
    commands += WithDebugCommand.withDebug,
    (Test / fork) := true,
    (Test / parallelExecution) := false,
    (Test / javaOptions) ++=
      Seq(
        "-Dpolyglot.engine.WarnInterpreterOnly=false",
        "-Dpolyglotimpl.DisableClassPathIsolation=true"
      ),
    // Append enso language on the class-path
    (Test / unmanagedClasspath) :=
      (LocalProject("runtime-fat-jar") / Compile / fullClasspath).value
  )
  .dependsOn(`benchmarks-common`)
  .dependsOn(`polyglot-api`)
  .dependsOn(runtime)

lazy val `std-benchmarks` = (project in file("std-bits/benchmarks"))
  .enablePlugins(JPMSPlugin)
  .settings(
    frgaalJavaCompilerSetting,
    annotationProcSetting,
    libraryDependencies ++= GraalVM.modules ++ GraalVM.langsPkgs ++ Seq(
      "org.openjdk.jmh"      % "jmh-core"                 % jmhVersion,
      "org.openjdk.jmh"      % "jmh-generator-annprocess" % jmhVersion,
      "org.graalvm.polyglot" % "polyglot"                 % graalMavenPackagesVersion,
      "org.slf4j"            % "slf4j-api"                % slf4jVersion,
      "org.slf4j"            % "slf4j-nop"                % slf4jVersion
    ),
    commands += WithDebugCommand.withDebug
  )
  .settings(
    parallelExecution := false,
    run / fork := true,
    run / connectInput := true,
    mainClass :=
      (LocalProject("bench-processor") / mainClass).value,
    Compile / compile := (Compile / compile)
      .dependsOn(`runtime-fat-jar` / assembly)
      .value,
    Compile / javacOptions ++= Seq(
      "-Xlint:unchecked",
      "-J-Dpolyglotimpl.DisableClassPathIsolation=true",
      "-J-Dpolyglot.engine.WarnInterpreterOnly=false"
    ),
    moduleDependencies := {
      componentModulesIds.value ++ Seq(
        "org.slf4j" % "slf4j-nop" % slf4jVersion
      )
    },
    addModules := {
      val runtimeModuleName = (`runtime-fat-jar` / javaModuleName).value
      Seq(runtimeModuleName)
    },
    addExports := {
      Map("org.slf4j.nop/org.slf4j.nop" -> Seq("org.slf4j"))
    },
    javaOptions ++= {
      Seq(
        // To enable logging in benchmarks, add ch.qos.logback module on the modulePath
        "-Dslf4j.provider=org.slf4j.nop.NOPServiceProvider"
      )
    },
    javaOptions ++= benchOnlyOptions
  )
  .settings(
    bench := Def
      .task {
        (Compile / run).toTask("").tag(Exclusive).value
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
        (Compile / run).toTask(" " + name).value
      }
    }.evaluated
  )
  .dependsOn(`bench-processor`)
  .dependsOn(`runtime-fat-jar`)
  .dependsOn(`std-table` % "provided")
  .dependsOn(`std-base` % "provided")
  .dependsOn(`benchmark-java-helpers` % "provided")

lazy val editions = project
  .in(file("lib/scala/editions"))
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    libraryDependencies ++= Seq(
      "io.circe"      %% "circe-core" % circeVersion     % "provided",
      "org.yaml"       % "snakeyaml"  % snakeyamlVersion % "provided",
      "org.scalatest" %% "scalatest"  % scalatestVersion % Test
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
  .dependsOn(semver)
  .dependsOn(testkit % Test)

lazy val semver = project
  .in(file("lib/scala/semver"))
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    libraryDependencies ++= Seq(
      "io.circe"      %% "circe-core"      % circeVersion     % "provided",
      "org.yaml"       % "snakeyaml"       % snakeyamlVersion % "provided",
      "org.scalatest" %% "scalatest"       % scalatestVersion % Test,
      "junit"          % "junit"           % junitVersion     % Test,
      "com.github.sbt" % "junit-interface" % junitIfVersion   % Test
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
  .dependsOn(`scala-yaml`)

lazy val downloader = (project in file("lib/scala/downloader"))
  .settings(
    frgaalJavaCompilerSetting,
    // Fork the tests to make sure that the withDebug command works (we can
    // attach debugger to the subprocess)
    (Test / fork) := true,
    commands += WithDebugCommand.withDebug,
    version := "0.1",
    libraryDependencies ++= circe ++ Seq(
      "com.typesafe.scala-logging" %% "scala-logging"    % scalaLoggingVersion,
      "commons-io"                  % "commons-io"       % commonsIoVersion,
      "org.apache.commons"          % "commons-compress" % commonsCompressVersion,
      "org.scalatest"              %% "scalatest"        % scalatestVersion % Test,
      "junit"                       % "junit"            % junitVersion     % Test,
      "com.github.sbt"              % "junit-interface"  % junitIfVersion   % Test,
      "org.hamcrest"                % "hamcrest-all"     % hamcrestVersion  % Test
    )
  )
  .dependsOn(cli)
  .dependsOn(`http-test-helper` % "test->test")
  .dependsOn(testkit % Test)

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
    frgaalJavaCompilerSetting,
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % circeVersion % "provided"
    )
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
    commands += WithDebugCommand.withDebug,
    Test / javaOptions ++= testLogProviderOptions,
    Test / test := (Test / test).tag(simpleLibraryServerTag).value,
    libraryDependencies ++= Seq(
      "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion,
      "org.scalatest"              %% "scalatest"     % scalatestVersion % Test
    )
  )
  .dependsOn(`library-manager`)
  .dependsOn(`process-utils`)
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
      "org.scalatest"              %% "scalatest"     % scalatestVersion % Test
    )
  )
  .dependsOn(`distribution-manager`)
  .dependsOn(`connected-lock-manager-server` % "test->test")
  .dependsOn(`polyglot-api`)

/** Unlike `connected-lock-manager` project, has a dependency on akka.
  */
lazy val `connected-lock-manager-server` = project
  .in(file("lib/scala/connected-lock-manager-server"))
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
      "org.apache.commons"          % "commons-compress" % commonsCompressVersion,
      "org.scalatest"              %% "scalatest"        % scalatestVersion % Test,
      akkaHttp
    )
  )
  .dependsOn(pkg)
  .dependsOn(downloader)
  .dependsOn(cli)
  .dependsOn(`process-utils`)
  .dependsOn(`version-output`)
  .dependsOn(`edition-updater`)
  .dependsOn(`distribution-manager`)

/** `process-utils` provides utilities for correctly managing process execution such as providing
  *  handlers for its stdout/stderr.
  */
lazy val `process-utils` = project
  .in(file("lib/scala/process-utils"))
  .settings(
    frgaalJavaCompilerSetting
  )

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
val `std-snowflake-polyglot-root` =
  stdLibComponentRoot("Snowflake") / "polyglot" / "java"
val `std-microsoft-polyglot-root` =
  stdLibComponentRoot("Microsoft") / "polyglot" / "java"
val `std-tableau-polyglot-root` =
  stdLibComponentRoot("Tableau") / "polyglot" / "java"

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
      "org.graalvm.polyglot"       % "polyglot"                % graalMavenPackagesVersion,
      "org.netbeans.api"           % "org-openide-util-lookup" % netbeansApiVersion % "provided",
      "com.fasterxml.jackson.core" % "jackson-databind"        % jacksonVersion
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
      "com.ibm.icu"          % "icu4j"    % icuVersion,
      "org.graalvm.polyglot" % "polyglot" % graalMavenPackagesVersion % "provided"
    )
  )

lazy val `enso-test-java-helpers` = project
  .in(file("test/Base_Tests/polyglot-sources/enso-test-java-helpers"))
  .settings(
    frgaalJavaCompilerSetting,
    autoScalaLibrary := false,
    Compile / packageBin / artifactPath :=
      file("test/Base_Tests/polyglot/java/helpers.jar"),
    libraryDependencies ++= Seq(
      "org.graalvm.polyglot" % "polyglot" % graalMavenPackagesVersion % "provided"
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
      "org.graalvm.polyglot" % "polyglot" % graalMavenPackagesVersion % "provided"
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
      "org.graalvm.polyglot"     % "polyglot"                % graalMavenPackagesVersion % "provided",
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
      "org.graalvm.polyglot" % "polyglot"                % graalMavenPackagesVersion % "provided",
      "org.netbeans.api"     % "org-openide-util-lookup" % netbeansApiVersion        % "provided",
      "org.openpnp"          % "opencv"                  % "4.7.0-0"
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
      "com.google.api-client" % "google-api-client"          % "2.2.0" exclude ("com.google.code.findbugs", "jsr305"),
      "com.google.apis"       % "google-api-services-sheets" % "v4-rev612-1.25.0" exclude ("com.google.code.findbugs", "jsr305"),
      "com.google.analytics"  % "google-analytics-data"      % "0.44.0" exclude ("com.google.code.findbugs", "jsr305")
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
      "org.graalvm.polyglot" % "polyglot"                % graalMavenPackagesVersion % "provided",
      "org.netbeans.api"     % "org-openide-util-lookup" % netbeansApiVersion        % "provided",
      "org.xerial"           % "sqlite-jdbc"             % sqliteVersion,
      "org.postgresql"       % "postgresql"              % "42.4.0"
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
      "software.amazon.awssdk" % "bom"                     % awsJavaSdkV2Version,
      "software.amazon.awssdk" % "s3"                      % awsJavaSdkV2Version,
      "software.amazon.awssdk" % "sso"                     % awsJavaSdkV2Version,
      "software.amazon.awssdk" % "ssooidc"                 % awsJavaSdkV2Version
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

lazy val `std-snowflake` = project
  .in(file("std-bits") / "snowflake")
  .settings(
    frgaalJavaCompilerSetting,
    autoScalaLibrary := false,
    Compile / compile / compileInputs := (Compile / compile / compileInputs)
      .dependsOn(SPIHelpers.ensureSPIConsistency)
      .value,
    Compile / packageBin / artifactPath :=
      `std-snowflake-polyglot-root` / "std-snowflake.jar",
    libraryDependencies ++= Seq(
      "org.netbeans.api" % "org-openide-util-lookup" % netbeansApiVersion % "provided",
      "net.snowflake"    % "snowflake-jdbc"          % snowflakeJDBCVersion
    ),
    Compile / packageBin := Def.task {
      val result = (Compile / packageBin).value
      val _ = StdBits
        .copyDependencies(
          `std-snowflake-polyglot-root`,
          Seq("std-snowflake.jar"),
          ignoreScalaLibrary = true
        )
        .value
      result
    }.value
  )
  .dependsOn(`std-base` % "provided")
  .dependsOn(`std-table` % "provided")
  .dependsOn(`std-database` % "provided")

lazy val `std-microsoft` = project
  .in(file("std-bits") / "microsoft")
  .settings(
    frgaalJavaCompilerSetting,
    autoScalaLibrary := false,
    Compile / compile / compileInputs := (Compile / compile / compileInputs)
      .dependsOn(SPIHelpers.ensureSPIConsistency)
      .value,
    Compile / packageBin / artifactPath :=
      `std-microsoft-polyglot-root` / "std-microsoft.jar",
    libraryDependencies ++= Seq(
      "org.netbeans.api"        % "org-openide-util-lookup" % netbeansApiVersion % "provided",
      "com.microsoft.sqlserver" % "mssql-jdbc"              % mssqlserverJDBCVersion
    ),
    Compile / packageBin := Def.task {
      val result = (Compile / packageBin).value
      val _ = StdBits
        .copyDependencies(
          `std-microsoft-polyglot-root`,
          Seq("std-microsoft.jar"),
          ignoreScalaLibrary = true
        )
        .value
      result
    }.value
  )
  .dependsOn(`std-base` % "provided")
  .dependsOn(`std-table` % "provided")
  .dependsOn(`std-database` % "provided")

lazy val `std-tableau` = project
  .in(file("std-bits") / "tableau")
  .settings(
    frgaalJavaCompilerSetting,
    autoScalaLibrary := false,
    unmanagedExternalZip := {
      val platform = if (Platform.isWindows) {
        "windows"
      } else if (Platform.isMacOS) {
        "macos"
      } else if (Platform.isLinux) {
        "linux"
      }
      val arch = if (Platform.isArm64) {
        "arm64"
      } else {
        "x86_64"
      }
      new URI(
        s"https://downloads.tableau.com/tssoftware/tableauhyperapi-java-$platform-$arch-release-main.$tableauVersion.zip"
      ).toURL()
    },
    fetchZipToUnmanaged := {
      val unmanagedDirectory = (Compile / unmanagedBase).value
      val logger             = state.value.log
      if (IO.listFiles(unmanagedDirectory).size < 2) { // Heuristic, should have at least hyperapi jar and os-specific one.
        logger.log(
          Level.Info,
          "std-tableau's unmanaged dependencies are not up-to-date. fetching..."
        )
        unmanagedDirectory.mkdirs()
        val unmanagedPath = unmanagedDirectory.toPath
        IO.withTemporaryDirectory(
          tmp => {
            val files = IO.unzipURL(
              unmanagedExternalZip.value,
              tmp,
              f =>
                f.endsWith(".jar") && !f.contains("gradle") && !f.contains(
                  "javadoc"
                ) && !f.contains("jna")
            )
            files.map { f =>
              IO.move(f, unmanagedPath.resolve(f.getName).toFile)
              Attributed.blank(unmanagedPath.resolve(f.getName).toFile)
            }.toSeq
          },
          keepDirectory = false
        )
      } else {
        Seq[Attributed[File]]()
      }
    },
    Compile / unmanagedClasspath := Def.task {
      val additionalFiles: Seq[Attributed[File]] = fetchZipToUnmanaged.value
      val result                                 = (Compile / unmanagedClasspath).value
      result ++ additionalFiles
    }.value,
    Compile / unmanagedJars := (Compile / unmanagedJars)
      .dependsOn(fetchZipToUnmanaged)
      .value,
    Compile / packageBin / artifactPath :=
      `std-tableau-polyglot-root` / "std-tableau.jar",
    libraryDependencies ++= Seq(
      "org.netbeans.api" % "org-openide-util-lookup" % netbeansApiVersion % "provided",
      "net.java.dev.jna" % "jna-platform"            % jnaVersion
    ),
    Compile / packageBin := Def.task {
      val result = (Compile / packageBin).value
      val _ = StdBits
        .copyDependencies(
          `std-tableau-polyglot-root`,
          Seq("std-tableau.jar"),
          ignoreScalaLibrary = true
        )
        .value
      result
    }.value
  )
  .dependsOn(`std-base` % "provided")
  .dependsOn(`std-table` % "provided")

lazy val fetchZipToUnmanaged =
  taskKey[Seq[Attributed[File]]](
    "Download zip file from an `unmanagedExternalZip` url and unpack jars to unmanaged libs directory"
  )
lazy val unmanagedExternalZip =
  settingKey[URL]("URL to zip file with dependencies")

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
  val modulesToCopy = componentModulesPaths.value
  val root          = engineDistributionRoot.value
  val log           = streams.value.log
  val cacheFactory  = streams.value.cacheStoreFactory
  DistributionPackage.createEnginePackage(
    distributionRoot    = root,
    cacheFactory        = cacheFactory,
    log                 = log,
    jarModulesToCopy    = modulesToCopy,
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

ThisBuild / engineDistributionRoot := {
  engineDistributionRoot.value
}

lazy val buildEngineDistributionNoIndex =
  taskKey[Unit]("Builds the engine distribution without generating indexes")
buildEngineDistributionNoIndex := {
  val _ = (`engine-runner` / assembly).value
  updateLibraryManifests.value
  val modulesToCopy = componentModulesPaths.value
  val root          = engineDistributionRoot.value
  val log           = streams.value.log
  val cacheFactory  = streams.value.cacheStoreFactory
  DistributionPackage.createEnginePackage(
    distributionRoot    = root,
    cacheFactory        = cacheFactory,
    log                 = log,
    jarModulesToCopy    = modulesToCopy,
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

// This makes the buildEngineDistributionNoIndex task usable as a dependency
// of other tasks.
ThisBuild / buildEngineDistributionNoIndex := {
  buildEngineDistributionNoIndex.result.value
}

lazy val runEngineDistribution =
  inputKey[Unit]("Run or --debug the engine distribution with arguments")
runEngineDistribution := {
  buildEngineDistributionNoIndex.value
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
    "Microsoft",
    "Snowflake",
    "Table"
  ) ++ allStdBitsSuffix
val allStdBits: Parser[String] =
  stdBitsProjects.map(v => v: Parser[String]).reduce(_ | _)

lazy val `http-test-helper` = project
  .in(file("tools") / "http-test-helper")
  .settings(
    customFrgaalJavaCompilerSettings(targetJdk = "21"),
    autoScalaLibrary := false,
    Compile / javacOptions ++= Seq("-Xlint:all"),
    Compile / run / mainClass := Some("org.enso.shttp.HTTPTestHelperServer"),
    libraryDependencies ++= Seq(
      "org.apache.commons"         % "commons-text"     % commonsTextVersion,
      "org.apache.httpcomponents"  % "httpclient"       % httpComponentsVersion,
      "com.fasterxml.jackson.core" % "jackson-databind" % jacksonVersion
    ),
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", "MANIFEST.MF", xs @ _*) =>
        MergeStrategy.discard
      case PathList(xs @ _*) if xs.last.contains("module-info") =>
        MergeStrategy.discard
      case _ => MergeStrategy.first
    },
    assembly / mainClass := (Compile / run / mainClass).value,
    (Compile / run / fork) := true,
    (Compile / run / connectInput) := true
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
    case "Snowflake" =>
      (`std-snowflake` / Compile / packageBin).value
    case "Microsoft" =>
      (`std-microsoft` / Compile / packageBin).value
    case "Tableau" =>
      (`std-tableau` / Compile / packageBin).value
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
      (`std-snowflake` / Compile / packageBin).value
      (`std-microsoft` / Compile / packageBin).value
      (`std-tableau` / Compile / packageBin).value
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
  val log      = streams.value.log
  val distOs   = "DIST_OS"
  val distArch = "DIST_ARCH"
  val osName   = "os.name"
  val archName = "os.arch"
  val distName = sys.env.get(distOs).getOrElse {
    val name = sys.props(osName).takeWhile(!_.isWhitespace)
    if (sys.env.contains("CI")) {
      log.warn(
        s"$distOs env var is empty. Fallback to system property $osName=$name."
      )
    }
    name
  }
  val arch = sys.env.get(distArch).orElse(sys.env.get(archName))
  val os = DistributionPackage.OS(distName, arch).getOrElse {
    throw new RuntimeException(s"Failed to determine OS: $distName.")
  }
  packageBuilder.createGraalPackage(
    log,
    os,
    os.archs.head
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
  val runnerCp   = (LocalProject("engine-runner") / Runtime / fullClasspath).value
  val runtimeCp  = (LocalProject("runtime") / Runtime / fullClasspath).value
  val fullCp     = (runnerCp ++ runtimeCp).distinct
  val modulePath = componentModulesPaths.value
  val runnerJar  = (LocalProject("engine-runner") / assembly).value
  val javaOpts = Seq(
    "-Denso.runner=" + runnerJar.getAbsolutePath,
    "--module-path",
    modulePath.map(_.getAbsolutePath).mkString(File.pathSeparator),
    "-m",
    "org.enso.runtime/org.enso.EngineRunnerBootLoader"
  )
  LibraryManifestGenerator.generateManifests(
    libraries,
    file("distribution"),
    log,
    javaOpts,
    cacheFactory
  )
}
