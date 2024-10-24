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
import PackageListPlugin.autoImport._

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
      `runtime-and-langs`,
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

lazy val frgaalShouldNotLimitModules = Def.settingKey[Boolean](
  "Whether --limit-modules cmd line option should be passed to the java process that runs " +
  "the frgaal compiler"
)

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
    `akka-native`,
    `akka-wrapper`,
    `benchmark-java-helpers`,
    `benchmarks-common`,
    `bench-processor`,
    cli,
    `common-polyglot-core-utils`,
    `connected-lock-manager`,
    `connected-lock-manager-server`,
    `desktop-environment`,
    `directory-watcher-wrapper`,
    `distribution-manager`,
    downloader,
    editions,
    `edition-updater`,
    `edition-uploader`,
    `engine-common`,
    `engine-runner-common`,
    `engine-runner`,
    `enso-test-java-helpers`,
    `exploratory-benchmark-java-helpers`,
    `fansi-wrapper`,
    filewatcher,
    `http-test-helper`,
    `interpreter-dsl`,
    `interpreter-dsl-test`,
    `jna-wrapper`,
    `json-rpc-server-test`,
    `json-rpc-server`,
    `language-server`,
    `language-server-deps-wrapper`,
    launcher,
    `library-manager`,
    `library-manager-test`,
    `locking-test-helper`,
    `logging-config`,
    `logging-service`,
    `logging-service-logback`,
    `logging-truffle-connector`,
    `logging-utils`,
    `logging-utils-akka`,
    `persistance`,
    `persistance-dsl`,
    pkg,
    `polyglot-api`,
    `polyglot-api-macros`,
    `process-utils`,
    `profiling-utils`,
    `project-manager`,
    `refactoring-utils`,
    runtime,
    `runtime-and-langs`,
    `runtime-benchmarks`,
    `runtime-compiler`,
    `runtime-integration-tests`,
    `runtime-parser`,
    `runtime-language-arrow`,
    `runtime-language-epb`,
    `runtime-instrument-common`,
    `runtime-instrument-id-execution`,
    `runtime-instrument-repl-debugger`,
    `runtime-instrument-runtime-server`,
    `runtime-integration-tests`,
    `runtime-parser`,
    `runtime-suggestions`,
    `runtime-version-manager`,
    `runtime-version-manager-test`,
    `runtime-test-instruments`,
    `scala-libs-wrapper`,
    `scala-yaml`,
    searcher,
    semver,
    `std-aws`,
    `std-base`,
    `std-benchmarks`,
    `std-database`,
    `std-google-api`,
    `std-image`,
    `std-microsoft`,
    `std-snowflake`,
    `std-table`,
    `std-tableau`,
    `syntax-rust-definition`,
    `task-progress-notifications`,
    testkit,
    `test-utils`,
    `text-buffer`,
    `version-output`,
    `ydoc-server`,
    `zio-wrapper`
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
val reactiveStreamsVersion    = "1.0.3"
val sprayJsonVersion          = "1.3.6"
val logbackClassicVersion     = JPMSUtils.logbackClassicVersion
val javaDiffVersion           = "4.12"
val logbackPkg = Seq(
  "ch.qos.logback" % "logback-classic" % logbackClassicVersion,
  "ch.qos.logback" % "logback-core"    % logbackClassicVersion
)
val akkaActor   = akkaPkg("actor")
val akkaStream  = akkaPkg("stream")
val akkaTestkit = akkaPkg("testkit")
val akkaSLF4J   = akkaPkg("slf4j")
val akkaHttp    = akkaHTTPPkg("http")
val logbackTest = logbackPkg.map(_ % Test)
val akka =
  Seq(
    akkaActor,
    akkaStream,
    akkaHttp
  )

// === Cats ===================================================================

val catsVersion       = "2.10.0"
val jawnParserVersion = "1.5.1"

// === Circe ==================================================================

val circeVersion              = "0.14.7"
val circeGenericExtrasVersion = "0.14.3"
val circe = Seq("circe-core", "circe-generic", "circe-parser")
  .map("io.circe" %% _ % circeVersion)
val snakeyamlVersion = "2.3"

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
val helidonVersion = "4.0.8"
val helidon = Seq(
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
  "io.helidon.webclient"       % "helidon-webclient"           % helidonVersion,
  "io.helidon.webclient"       % "helidon-webclient-api"       % helidonVersion,
  "io.helidon.webclient"       % "helidon-webclient-http1"     % helidonVersion,
  "io.helidon.webclient"       % "helidon-webclient-websocket" % helidonVersion,
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
val jaActivationVersion = "2.1.0"

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
val scalaLibrary = Seq(
  "org.scala-lang" % "scala-library" % scalacVersion
)
val scalaParserCombinatorsVersion = "1.1.2"
val scalaJavaCompatVersion        = "1.0.0"
val scalaCollectionCompatVersion  = "2.8.1"

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

val zioVersion             = "2.0.14"
val zioInteropCatsVersion  = "23.0.0.6"
val zioIzumiReflectVersion = "2.3.8"
val zio = Seq(
  "dev.zio" %% "zio"              % zioVersion,
  "dev.zio" %% "zio-interop-cats" % zioInteropCatsVersion
)

// === Sentry =================================================================

val ioSentryVersion = "6.28.0"
val ioSentry = Seq(
  "io.sentry" % "sentry-logback" % ioSentryVersion,
  "io.sentry" % "sentry"         % ioSentryVersion
)

// === Bouncy Castle ==========================================================

val bouncyCastleVersion = "1.76"
val bouncyCastle = Seq(
  "org.bouncycastle" % "bcutil-jdk18on" % bouncyCastleVersion,
  "org.bouncycastle" % "bcpkix-jdk18on" % bouncyCastleVersion,
  "org.bouncycastle" % "bcprov-jdk18on" % bouncyCastleVersion
)

// === Google =================================================================
val googleApiClientVersion         = "2.2.0"
val googleApiServicesSheetsVersion = "v4-rev612-1.25.0"
val googleAnalyticsDataVersion     = "0.44.0"

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
val sqliteVersion           = "3.46.1.0"
val tikaVersion             = "2.4.1"
val typesafeConfigVersion   = "1.4.2"
val junitVersion            = "4.13.2"
val junitIfVersion          = "0.13.2"
val hamcrestVersion         = "1.3"
val netbeansApiVersion      = "RELEASE180"
val opencvVersion           = "4.7.0-0"
val fansiVersion            = "0.4.0"
val httpComponentsVersion   = "4.4.1"
val apacheArrowVersion      = "14.0.1"
val snowflakeJDBCVersion    = "3.15.0"
val mssqlserverJDBCVersion  = "12.6.2.jre11"
val jsoniterVersion         = "2.28.5"
val jnaVersion              = "5.14.0"
val googleProtobufVersion   = "3.25.1"
val shapelessVersion        = "2.3.10"
val postgresVersion         = "42.4.0"

// ============================================================================
// === Utility methods =====================================================
// ============================================================================

// TODO: Remove
lazy val componentModulesIds =
  taskKey[Seq[ModuleID]](
    "Gather all sbt module IDs that will be put on the module-path for the engine runner"
  )
(ThisBuild / componentModulesIds) := {
  GraalVM.modules ++ GraalVM.langsPkgs ++ GraalVM.toolsPkgs ++ helidon ++ logbackPkg ++ Seq(
    "org.slf4j"        % "slf4j-api"                    % slf4jVersion,
    "org.netbeans.api" % "org-netbeans-modules-sampler" % netbeansApiVersion,
    (`runtime-language-arrow` / projectID).value,
    (`syntax-rust-definition` / projectID).value,
    (`ydoc-server` / projectID).value,
    (`profiling-utils` / projectID).value
  )
}

lazy val componentModulesPaths =
  taskKey[Seq[File]](
    "Gathers all component modules (Jar archives that should be put on module-path" +
    " as files"
  )
(ThisBuild / componentModulesPaths) := {
  val runnerCp      = (`engine-runner` / Runtime / fullClasspath).value
  val runtimeCp     = (`runtime` / Runtime / fullClasspath).value
  val langServerCp  = (`language-server` / Runtime / fullClasspath).value
  val akkaWrapperCp = (`akka-wrapper` / Compile / fullClasspath).value
  val fullCp        = (runnerCp ++ runtimeCp ++ langServerCp ++ akkaWrapperCp).distinct
  val log           = streams.value.log
  val thirdPartyModIds =
    GraalVM.modules ++
    GraalVM.langsPkgs ++
    GraalVM.toolsPkgs ++
    helidon ++
    scalaLibrary ++
    ioSentry ++
    logbackPkg ++
    Seq(
      "org.scala-lang"         % "scala-reflect"                % scalacVersion,
      "org.netbeans.api"       % "org-openide-util-lookup"      % netbeansApiVersion,
      "org.netbeans.api"       % "org-netbeans-modules-sampler" % netbeansApiVersion,
      "com.google.flatbuffers" % "flatbuffers-java"             % flatbuffersVersion,
      "com.google.protobuf"    % "protobuf-java"                % googleProtobufVersion,
      "commons-cli"            % "commons-cli"                  % commonsCliVersion,
      "commons-io"             % "commons-io"                   % commonsIoVersion,
      "org.yaml"               % "snakeyaml"                    % snakeyamlVersion,
      "org.eclipse.jgit"       % "org.eclipse.jgit"             % jgitVersion,
      "com.typesafe"           % "config"                       % typesafeConfigVersion,
      "org.reactivestreams"    % "reactive-streams"             % reactiveStreamsVersion,
      "org.jline"              % "jline"                        % jlineVersion,
      "org.apache.commons"     % "commons-lang3"                % commonsLangVersion,
      "org.apache.commons"     % "commons-compress"             % commonsCompressVersion,
      "org.apache.tika"        % "tika-core"                    % tikaVersion,
      "org.slf4j"              % "slf4j-api"                    % slf4jVersion,
      "org.yaml"               % "snakeyaml"                    % snakeyamlVersion,
      "com.ibm.icu"            % "icu4j"                        % icuVersion
    )
  val thirdPartyMods = JPMSUtils.filterModulesFromClasspath(
    fullCp,
    thirdPartyModIds,
    log,
    projName = moduleName.value,
    scalaBinaryVersion.value,
    shouldContainAll = true
  )
  val thirdPartyModFiles = thirdPartyMods.map(_.data)
  val ourMods = Seq(
    (`common-polyglot-core-utils` / Compile / exportedModuleBin).value,
    (`engine-common` / Compile / exportedModuleBin).value,
    (`engine-runner` / Compile / exportedModuleBin).value,
    (`engine-runner-common` / Compile / exportedModuleBin).value,
    (`polyglot-api` / Compile / exportedModuleBin).value,
    (`polyglot-api-macros` / Compile / exportedModuleBin).value,
    (`runtime` / Compile / exportedModuleBin).value,
    (`syntax-rust-definition` / Compile / exportedModuleBin).value,
    (`runtime-compiler` / Compile / exportedModuleBin).value,
    (`runtime-parser` / Compile / exportedModuleBin).value,
    (`runtime-suggestions` / Compile / exportedModuleBin).value,
    (`runtime-instrument-common` / Compile / exportedModuleBin).value,
    (`runtime-instrument-id-execution` / Compile / exportedModuleBin).value,
    (`runtime-instrument-repl-debugger` / Compile / exportedModuleBin).value,
    (`runtime-instrument-runtime-server` / Compile / exportedModuleBin).value,
    (`runtime-language-arrow` / Compile / exportedModuleBin).value,
    (`runtime-language-epb` / Compile / exportedModuleBin).value,
    (`persistance` / Compile / exportedModuleBin).value,
    (`cli` / Compile / exportedModuleBin).value,
    (`json-rpc-server` / Compile / exportedModuleBin).value,
    (`connected-lock-manager` / Compile / exportedModuleBin).value,
    (`connected-lock-manager-server` / Compile / exportedModuleBin).value,
    (`distribution-manager` / Compile / exportedModuleBin).value,
    (`downloader` / Compile / exportedModuleBin).value,
    (`filewatcher` / Compile / exportedModuleBin).value,
    (`editions` / Compile / exportedModuleBin).value,
    (`language-server` / Compile / exportedModuleBin).value,
    (`library-manager` / Compile / exportedModuleBin).value,
    (`akka-wrapper` / Compile / exportedModuleBin).value,
    (`zio-wrapper` / Compile / exportedModuleBin).value,
    (`language-server-deps-wrapper` / Compile / exportedModuleBin).value,
    (`directory-watcher-wrapper` / Compile / exportedModuleBin).value,
    (`jna-wrapper` / Compile / exportedModuleBin).value,
    (`ydoc-server` / Compile / exportedModuleBin).value,
    (`library-manager` / Compile / exportedModuleBin).value,
    (`logging-config` / Compile / exportedModuleBin).value,
    (`logging-utils` / Compile / exportedModuleBin).value,
    (`logging-utils-akka` / Compile / exportedModuleBin).value,
    (`logging-service` / Compile / exportedModuleBin).value,
    (`logging-service-logback` / Compile / exportedModuleBin).value,
    (`pkg` / Compile / exportedModuleBin).value,
    (`refactoring-utils` / Compile / exportedModuleBin).value,
    (`task-progress-notifications` / Compile / exportedModuleBin).value,
    (`semver` / Compile / exportedModuleBin).value,
    (`searcher` / Compile / exportedModuleBin).value,
    (`text-buffer` / Compile / exportedModuleBin).value,
    (`version-output` / Compile / exportedModuleBin).value,
    (`scala-yaml` / Compile / exportedModuleBin).value,
    (`scala-libs-wrapper` / Compile / exportedModuleBin).value,
    (`fansi-wrapper` / Compile / exportedModuleBin).value,
    (`edition-updater` / Compile / exportedModuleBin).value,
    (`profiling-utils` / Compile / exportedModuleBin).value
  )
  ourMods ++ thirdPartyModFiles
}

/** Common settings for our wrappers of some *problematic* dependencies that are not
  * compatible with the JPMS system, i.e., these dependencies cannot be put on module-path.
  * These projects contain only single `module-info.java` source.
  * Before this source is compiled, all the dependencies are gathered via the `assembly`
  * task into a Jar.
  * The `module-info.java` exports all the packages from these dependencies.
  * Note that this is the recommended way how to handle dependencies that are not
  * JPMS-friendly.
  *
  * `exportedModule` of these projects return path to the assembled modular Jar.
  * The projects should define:
  * - `moduleDependencies`
  * - `patchModules`
  * - `assembly / assemblyExcludedJars`
  */
lazy val modularFatJarWrapperSettings = frgaalJavaCompilerSetting ++ Seq(
  Compile / forceModuleInfoCompilation := true,
  Compile / exportedModuleBin := assembly
    .dependsOn(Compile / compileModuleInfo)
    .value,
  Compile / exportedModule := (Compile / exportedModuleBin).value
)

// ============================================================================
// === Internal Libraries =====================================================
// ============================================================================

lazy val `text-buffer` = project
  .in(file("lib/scala/text-buffer"))
  .enablePlugins(JPMSPlugin)
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    scalaModuleDependencySetting,
    mixedJavaScalaProjectSetting,
    commands += WithDebugCommand.withDebug,
    javaModuleName := "org.enso.text.buffer",
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
    libraryDependencies ++= Seq(
      "org.slf4j" % "slf4j-api" % slf4jVersion
    ),
    Compile / moduleDependencies ++= Seq(
      "org.slf4j" % "slf4j-api" % slf4jVersion
    ),
    javaModuleName := "org.enso.syntax",
    Compile / sourceGenerators += generateParserJavaSources,
    Compile / resourceGenerators += generateRustParserLib,
    Compile / javaSource := baseDirectory.value / "generate-java" / "java",
    Compile / compile / javacOptions ++= Seq("-source", "11", "-target", "11")
  )

lazy val `scala-yaml` = (project in file("lib/scala/yaml"))
  .enablePlugins(JPMSPlugin)
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    scalaModuleDependencySetting,
    mixedJavaScalaProjectSetting,
    libraryDependencies ++= Seq(
      "org.yaml" % "snakeyaml" % snakeyamlVersion % "provided"
    ),
    Compile / moduleDependencies ++= Seq(
      "org.yaml" % "snakeyaml" % snakeyamlVersion
    ),
    Compile / internalModuleDependencies := Seq(
      (`scala-libs-wrapper` / Compile / exportedModule).value
    )
  )
  .dependsOn(`scala-libs-wrapper`)

lazy val pkg = (project in file("lib/scala/pkg"))
  .enablePlugins(JPMSPlugin)
  .settings(
    frgaalJavaCompilerSetting,
    scalaModuleDependencySetting,
    compileOrder := CompileOrder.ScalaThenJava,
    version := "0.1",
    Compile / run / mainClass := Some("org.enso.pkg.Main"),
    libraryDependencies ++= Seq(
      "io.circe"          %% "circe-core"       % circeVersion     % "provided",
      "org.yaml"           % "snakeyaml"        % snakeyamlVersion % "provided",
      "org.scalatest"     %% "scalatest"        % scalatestVersion % Test,
      "org.apache.commons" % "commons-compress" % commonsCompressVersion
    ),
    Compile / moduleDependencies ++= Seq(
      "org.apache.commons" % "commons-compress" % commonsCompressVersion,
      "org.yaml"           % "snakeyaml"        % snakeyamlVersion
    ),
    Compile / internalModuleDependencies := Seq(
      (`editions` / Compile / exportedModule).value,
      (`semver` / Compile / exportedModule).value,
      (`scala-yaml` / Compile / exportedModule).value,
      (`scala-libs-wrapper` / Compile / exportedModule).value
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
    Compile / moduleDependencies ++= {
      Seq(
        "org.slf4j"        % "slf4j-api"                    % slf4jVersion,
        "org.netbeans.api" % "org-netbeans-modules-sampler" % netbeansApiVersion
      )
    }
  )

lazy val `logging-utils` = project
  .in(file("lib/scala/logging-utils"))
  .enablePlugins(JPMSPlugin)
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    scalaModuleDependencySetting,
    compileOrder := CompileOrder.ScalaThenJava, // Note [JPMS Compile order]
    version := "0.1",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % scalatestVersion % Test,
      "org.slf4j"      % "slf4j-api" % slf4jVersion
    ) ++ logbackTest,
    Compile / moduleDependencies ++=
      Seq(
        "org.slf4j" % "slf4j-api" % slf4jVersion
      )
  )

lazy val `logging-service` = project
  .in(file("lib/scala/logging-service"))
  .enablePlugins(JPMSPlugin)
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    scalaModuleDependencySetting,
    version := "0.1",
    libraryDependencies ++= Seq(
      "org.slf4j"      % "slf4j-api" % slf4jVersion,
      "com.typesafe"   % "config"    % typesafeConfigVersion,
      "org.scalatest" %% "scalatest" % scalatestVersion % Test
    ),
    Compile / moduleDependencies ++= Seq(
      "org.slf4j" % "slf4j-api" % slf4jVersion
    ),
    Compile / internalModuleDependencies := Seq(
      (`logging-config` / Compile / exportedModule).value,
      (`logging-utils` / Compile / exportedModule).value
    )
  )
  .dependsOn(`logging-utils`)
  .dependsOn(`logging-config`)

lazy val `logging-config` = project
  .in(file("lib/scala/logging-config"))
  .enablePlugins(JPMSPlugin)
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    version := "0.1",
    libraryDependencies ++= Seq(
      "com.typesafe" % "config"    % typesafeConfigVersion,
      "org.slf4j"    % "slf4j-api" % slf4jVersion
    ),
    Compile / moduleDependencies ++= Seq(
      "com.typesafe" % "config"    % typesafeConfigVersion,
      "org.slf4j"    % "slf4j-api" % slf4jVersion
    )
  )

lazy val `logging-service-logback` = project
  .in(file("lib/scala/logging-service-logback"))
  .enablePlugins(JPMSPlugin)
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    version := "0.1",
    libraryDependencies ++= Seq(
      "org.slf4j"        % "slf4j-api"               % slf4jVersion,
      "org.scalatest"   %% "scalatest"               % scalatestVersion   % Test,
      "org.netbeans.api" % "org-openide-util-lookup" % netbeansApiVersion % "provided"
    ) ++ logbackPkg ++ ioSentry,
    Compile / moduleDependencies ++= logbackPkg ++ ioSentry ++ Seq(
      "org.slf4j"        % "slf4j-api"               % slf4jVersion,
      "org.netbeans.api" % "org-openide-util-lookup" % netbeansApiVersion % "provided"
    ),
    Compile / shouldCompileModuleInfoManually := true,
    Compile / internalModuleDependencies := Seq(
      (`logging-service` / Compile / exportedModule).value,
      (`logging-config` / Compile / exportedModule).value
    ),
    Test / moduleDependencies ++= scalaLibrary,
    Test / internalModuleDependencies := Seq(
      (Compile / exportedModule).value
    )
  )
  .dependsOn(`logging-config`)
  .dependsOn(`logging-service`)

lazy val `logging-utils-akka` = project
  .in(file("lib/scala/logging-utils-akka"))
  .enablePlugins(JPMSPlugin)
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    scalaModuleDependencySetting,
    version := "0.1",
    compileOrder := CompileOrder.ScalaThenJava,
    libraryDependencies ++= Seq(
      "org.slf4j"          % "slf4j-api"  % slf4jVersion,
      "com.typesafe.akka" %% "akka-actor" % akkaVersion
    ),
    Compile / moduleDependencies ++=
      Seq(
        "org.slf4j" % "slf4j-api" % slf4jVersion
      ),
    Compile / internalModuleDependencies := Seq(
      (`akka-wrapper` / Compile / exportedModule).value
    )
  )

lazy val filewatcher = project
  .in(file("lib/scala/filewatcher"))
  .enablePlugins(JPMSPlugin)
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    scalaModuleDependencySetting,
    compileOrder := CompileOrder.ScalaThenJava,
    version := "0.1",
    libraryDependencies ++= Seq(
      "io.methvin"     % "directory-watcher" % directoryWatcherVersion,
      "commons-io"     % "commons-io"        % commonsIoVersion,
      "org.slf4j"      % "slf4j-api"         % slf4jVersion,
      "org.scalatest" %% "scalatest"         % scalatestVersion % Test
    ),
    Compile / moduleDependencies ++= Seq(
      "org.slf4j" % "slf4j-api" % slf4jVersion
    ),
    Compile / internalModuleDependencies := Seq(
      (`directory-watcher-wrapper` / Compile / exportedModule).value
    ),
    Test / fork := true,
    Test / javaOptions ++= testLogProviderOptions
  )
  .dependsOn(testkit % Test)
  .dependsOn(`logging-service-logback` % "test->test")
  .dependsOn(`directory-watcher-wrapper`)

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

/** This is a simple wrapper for some Scala libraries that cannot be put directly
  * on module-path. For example because it's automatic module name cannot be derived:
  * {{{
  * $ jar --describe-module -f ./circe-core_2.13-0.14.7.jar
  * Unable to derive module descriptor for: ./circe-core_2.13-0.14.7.jar
  * circe.core.2.13: Invalid module name: '2' is not a Java identifier
  * }}}
  * This project contains only a single `module-info.java` that serves as the module
  * descriptor for these problematic dependencies.
  */
lazy val `scala-libs-wrapper` = project
  .in(file("lib/java/scala-libs-wrapper"))
  .enablePlugins(JPMSPlugin)
  .settings(
    modularFatJarWrapperSettings,
    scalaModuleDependencySetting,
    javaModuleName := "org.enso.scala.wrapper",
    libraryDependencies ++= circe ++ scalaCompiler ++ Seq(
      "com.typesafe.scala-logging"            %% "scala-logging"         % scalaLoggingVersion,
      "org.slf4j"                              % "slf4j-api"             % slf4jVersion,
      "org.typelevel"                         %% "cats-core"             % catsVersion,
      "org.jline"                              % "jline"                 % jlineVersion,
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % jsoniterVersion,
      "net.java.dev.jna"                       % "jna"                   % jnaVersion
    ),
    Compile / moduleDependencies ++= scalaLibrary ++ Seq(
      "org.scala-lang" % "scala-reflect" % scalacVersion,
      "org.jline"      % "jline"         % jlineVersion,
      "org.slf4j"      % "slf4j-api"     % slf4jVersion
    ),
    assembly / assemblyExcludedJars := {
      JPMSUtils.filterModulesFromClasspath(
        (Compile / fullClasspath).value,
        scalaLibrary ++
        scalaCompiler ++
        Seq(
          "org.scala-lang"            % "scala-reflect"   % scalacVersion,
          "org.slf4j"                 % "slf4j-api"       % slf4jVersion,
          "io.github.java-diff-utils" % "java-diff-utils" % javaDiffVersion,
          "org.jline"                 % "jline"           % jlineVersion,
          "net.java.dev.jna"          % "jna"             % jnaVersion
        ),
        streams.value.log,
        moduleName.value,
        scalaBinaryVersion.value,
        shouldContainAll = true
      )
    },
    // Patch this JPMS module such that the JVM thinks that all the Scala stuff
    // is part of this module
    Compile / patchModules := {
      val scalaLibs = JPMSUtils.filterModulesFromUpdate(
        update.value,
        Seq(
          "com.typesafe.scala-logging"            %% "scala-logging"         % scalaLoggingVersion,
          "io.circe"                              %% "circe-core"            % circeVersion,
          "io.circe"                              %% "circe-generic"         % circeVersion,
          "io.circe"                              %% "circe-parser"          % circeVersion,
          "io.circe"                              %% "circe-jawn"            % circeVersion,
          "io.circe"                              %% "circe-numbers"         % circeVersion,
          "org.typelevel"                         %% "cats-core"             % catsVersion,
          "org.typelevel"                         %% "cats-kernel"           % catsVersion,
          "org.typelevel"                         %% "jawn-parser"           % jawnParserVersion,
          "com.chuusai"                           %% "shapeless"             % shapelessVersion,
          "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % jsoniterVersion,
          "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core"   % jsoniterVersion
        ),
        streams.value.log,
        moduleName.value,
        scalaBinaryVersion.value,
        shouldContainAll = true
      )
      Map(
        javaModuleName.value -> scalaLibs
      )
    }
  )

/** Wrapper project for dependencies of `language-server` that cannot be used as
  * JPMS modules (cannot be put directly on module-path).
  */
lazy val `language-server-deps-wrapper` = project
  .in(file("lib/java/language-server-deps-wrapper"))
  .enablePlugins(JPMSPlugin)
  .settings(
    modularFatJarWrapperSettings,
    scalaModuleDependencySetting,
    libraryDependencies ++= Seq(
      "com.github.pureconfig" %% "pureconfig" % pureconfigVersion,
      "com.chuusai"           %% "shapeless"  % shapelessVersion,
      "com.typesafe"           % "config"     % typesafeConfigVersion
    ),
    javaModuleName := "org.enso.language.server.deps.wrapper",
    Compile / internalModuleDependencies := Seq(
      (`scala-libs-wrapper` / Compile / exportedModule).value
    ),
    assembly / assemblyExcludedJars := {
      JPMSUtils.filterModulesFromClasspath(
        (Compile / fullClasspath).value,
        scalaLibrary ++
        Seq(
          "com.chuusai" %% "shapeless" % shapelessVersion,
          "com.typesafe" % "config"    % typesafeConfigVersion
        ),
        streams.value.log,
        moduleName.value,
        scalaBinaryVersion.value,
        shouldContainAll = true
      )
    },
    Compile / patchModules := {
      val scalaLibs = JPMSUtils.filterModulesFromUpdate(
        update.value,
        Seq(
          "com.github.pureconfig" %% "pureconfig-core"    % pureconfigVersion,
          "com.github.pureconfig" %% "pureconfig-generic" % pureconfigVersion
        ),
        streams.value.log,
        moduleName.value,
        scalaBinaryVersion.value,
        shouldContainAll = true
      )
      Map(
        javaModuleName.value -> scalaLibs
      )
    }
  )

lazy val `jna-wrapper` = project
  .in(file("lib/java/jna-wrapper"))
  .enablePlugins(JPMSPlugin)
  .settings(
    modularFatJarWrapperSettings,
    autoScalaLibrary := false,
    libraryDependencies ++= Seq(
      "net.java.dev.jna" % "jna" % jnaVersion
    ),
    javaModuleName := "org.enso.jna.wrapper",
    Compile / patchModules := {
      val jna = JPMSUtils.filterModulesFromUpdate(
        update.value,
        scalaLibrary ++
        Seq(
          "net.java.dev.jna" % "jna" % jnaVersion
        ),
        streams.value.log,
        moduleName.value,
        scalaBinaryVersion.value,
        shouldContainAll = true
      )
      Map(
        javaModuleName.value -> jna
      )
    }
  )

lazy val `directory-watcher-wrapper` = project
  .in(file("lib/java/directory-watcher-wrapper"))
  .enablePlugins(JPMSPlugin)
  .settings(
    modularFatJarWrapperSettings,
    scalaModuleDependencySetting,
    libraryDependencies ++= Seq(
      "io.methvin"       % "directory-watcher" % directoryWatcherVersion,
      "org.slf4j"        % "slf4j-api"         % "1.7.36",
      "net.java.dev.jna" % "jna"               % jnaVersion
    ),
    javaModuleName := "org.enso.directory.watcher.wrapper",
    assembly / assemblyExcludedJars := {
      JPMSUtils.filterModulesFromClasspath(
        (Compile / dependencyClasspath).value,
        scalaLibrary ++
        Seq(
          "org.slf4j"        % "slf4j-api" % "1.7.36",
          "net.java.dev.jna" % "jna"       % jnaVersion
        ),
        streams.value.log,
        moduleName.value,
        scalaBinaryVersion.value,
        shouldContainAll = true
      )
    },
    Compile / moduleDependencies ++= Seq(
      "org.slf4j" % "slf4j-api" % "1.7.36"
    ),
    Compile / internalModuleDependencies := Seq(
      (`jna-wrapper` / Compile / exportedModule).value
    ),
    Compile / patchModules := {
      val scalaLibs = JPMSUtils.filterModulesFromUpdate(
        update.value,
        scalaLibrary ++
        Seq(
          "io.methvin" % "directory-watcher" % directoryWatcherVersion
        ),
        streams.value.log,
        moduleName.value,
        scalaBinaryVersion.value,
        shouldContainAll = true
      )
      Map(
        javaModuleName.value -> scalaLibs
      )
    }
  )

lazy val `fansi-wrapper` = project
  .in(file("lib/java/fansi-wrapper"))
  .enablePlugins(JPMSPlugin)
  .settings(
    modularFatJarWrapperSettings,
    scalaModuleDependencySetting,
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "fansi" % fansiVersion
    ),
    javaModuleName := "org.enso.fansi.wrapper",
    Compile / patchModules := {
      val scalaLibs = JPMSUtils.filterModulesFromUpdate(
        update.value,
        scalaLibrary ++
        Seq(
          "com.lihaoyi" %% "fansi" % fansiVersion
        ),
        streams.value.log,
        moduleName.value,
        scalaBinaryVersion.value,
        shouldContainAll = true
      )
      Map(
        javaModuleName.value -> scalaLibs
      )
    },
    assembly / assemblyExcludedJars := {
      JPMSUtils.filterModulesFromClasspath(
        (Compile / dependencyClasspath).value,
        scalaLibrary,
        streams.value.log,
        moduleName.value,
        scalaBinaryVersion.value,
        shouldContainAll = true
      )
    }
  )

/** JPMS module wrapper for Akka.
  */
lazy val `akka-wrapper` = project
  .in(file("lib/java/akka-wrapper"))
  .enablePlugins(JPMSPlugin)
  .settings(
    modularFatJarWrapperSettings,
    scalaModuleDependencySetting,
    libraryDependencies ++= akka ++ scalaLibrary ++ scalaCompiler ++ Seq(
      "org.scala-lang.modules"   %% "scala-parser-combinators" % scalaParserCombinatorsVersion,
      "org.scala-lang.modules"   %% "scala-java8-compat"       % scalaJavaCompatVersion,
      akkaURL                    %% "akka-http"                % akkaHTTPVersion,
      akkaURL                    %% "akka-http-core"           % akkaHTTPVersion,
      akkaURL                    %% "akka-slf4j"               % akkaVersion,
      akkaURL                    %% "akka-parsing"             % akkaHTTPVersion,
      akkaURL                    %% "akka-protobuf-v3"         % akkaVersion,
      akkaURL                    %% "akka-http-spray-json"     % akkaHTTPVersion,
      "com.typesafe"              % "config"                   % typesafeConfigVersion,
      "org.slf4j"                 % "slf4j-api"                % slf4jVersion,
      "com.google.protobuf"       % "protobuf-java"            % googleProtobufVersion,
      "io.github.java-diff-utils" % "java-diff-utils"          % javaDiffVersion,
      "org.reactivestreams"       % "reactive-streams"         % reactiveStreamsVersion,
      "org.jline"                 % "jline"                    % jlineVersion,
      "net.java.dev.jna"          % "jna"                      % jnaVersion,
      "io.spray"                 %% "spray-json"               % sprayJsonVersion
    ),
    javaModuleName := "org.enso.akka.wrapper",
    Compile / moduleDependencies ++= Seq(
      "com.google.protobuf" % "protobuf-java"    % googleProtobufVersion,
      "org.reactivestreams" % "reactive-streams" % reactiveStreamsVersion,
      "org.slf4j"           % "slf4j-api"        % slf4jVersion
    ),
    assembly / assemblyExcludedJars := {
      val excludedJars = JPMSUtils.filterModulesFromUpdate(
        update.value,
        scalaLibrary ++ scalaCompiler ++ Seq(
          "org.scala-lang.modules"   %% "scala-java8-compat" % scalaJavaCompatVersion,
          "org.slf4j"                 % "slf4j-api"          % slf4jVersion,
          "com.typesafe"              % "config"             % typesafeConfigVersion,
          "io.github.java-diff-utils" % "java-diff-utils"    % javaDiffVersion,
          "org.jline"                 % "jline"              % jlineVersion,
          "com.google.protobuf"       % "protobuf-java"      % googleProtobufVersion,
          "org.reactivestreams"       % "reactive-streams"   % reactiveStreamsVersion,
          "net.java.dev.jna"          % "jna"                % jnaVersion
        ),
        streams.value.log,
        moduleName.value,
        scalaBinaryVersion.value,
        shouldContainAll = true
      )
      excludedJars
        .map(Attributed.blank)
    },
    Compile / patchModules := {
      val scalaLibs = JPMSUtils.filterModulesFromUpdate(
        update.value,
        scalaLibrary ++ scalaCompiler ++
        Seq(
          "org.scala-lang.modules" %% "scala-parser-combinators" % scalaParserCombinatorsVersion,
          "com.typesafe"            % "config"                   % typesafeConfigVersion,
          "io.spray"               %% "spray-json"               % sprayJsonVersion,
          akkaURL                  %% "akka-actor"               % akkaVersion,
          akkaURL                  %% "akka-stream"              % akkaVersion,
          akkaURL                  %% "akka-http"                % akkaHTTPVersion,
          akkaURL                  %% "akka-http-core"           % akkaHTTPVersion,
          akkaURL                  %% "akka-http-spray-json"     % akkaHTTPVersion,
          akkaURL                  %% "akka-slf4j"               % akkaVersion,
          akkaURL                  %% "akka-parsing"             % akkaHTTPVersion,
          akkaURL                  %% "akka-protobuf-v3"         % akkaVersion
        ),
        streams.value.log,
        moduleName.value,
        scalaBinaryVersion.value,
        shouldContainAll = true
      )
      Map(
        javaModuleName.value -> scalaLibs
      )
    }
  )

lazy val `zio-wrapper` = project
  .in(file("lib/java/zio-wrapper"))
  .enablePlugins(JPMSPlugin)
  .settings(
    modularFatJarWrapperSettings,
    scalaModuleDependencySetting,
    javaModuleName := "org.enso.zio.wrapper",
    libraryDependencies ++= zio ++ Seq(
      "dev.zio" %% "zio-internal-macros"                       % zioVersion,
      "dev.zio" %% "zio-stacktracer"                           % zioVersion,
      "dev.zio" %% "izumi-reflect"                             % zioIzumiReflectVersion,
      "dev.zio" %% "izumi-reflect-thirdparty-boopickle-shaded" % zioIzumiReflectVersion
    ),
    assembly / assemblyExcludedJars := {
      val excludedJars = JPMSUtils.filterModulesFromUpdate(
        update.value,
        scalaLibrary ++
        scalaCompiler ++
        Seq("dev.zio" %% "zio-interop-cats" % zioInteropCatsVersion),
        streams.value.log,
        moduleName.value,
        scalaBinaryVersion.value,
        shouldContainAll = true
      )
      excludedJars
        .map(Attributed.blank)
    },
    Compile / patchModules := {
      val scalaLibs = JPMSUtils.filterModulesFromUpdate(
        update.value,
        scalaLibrary ++
        Seq(
          "dev.zio" %% "zio"                                       % zioVersion,
          "dev.zio" %% "zio-internal-macros"                       % zioVersion,
          "dev.zio" %% "zio-stacktracer"                           % zioVersion,
          "dev.zio" %% "izumi-reflect"                             % zioIzumiReflectVersion,
          "dev.zio" %% "izumi-reflect-thirdparty-boopickle-shaded" % zioIzumiReflectVersion
        ),
        streams.value.log,
        moduleName.value,
        scalaBinaryVersion.value,
        shouldContainAll = true
      )
      Map(
        javaModuleName.value -> scalaLibs
      )
    }
  )

lazy val cli = project
  .in(file("lib/scala/cli"))
  .enablePlugins(JPMSPlugin)
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    scalaModuleDependencySetting,
    compileOrder := CompileOrder.ScalaThenJava,
    version := "0.1",
    libraryDependencies ++= circe ++ Seq(
      "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion,
      "org.yaml"                    % "snakeyaml"     % snakeyamlVersion % "provided",
      "org.scalatest"              %% "scalatest"     % scalatestVersion % Test
    ),
    Compile / internalModuleDependencies := Seq(
      (`scala-libs-wrapper` / Compile / exportedModule).value,
      (`scala-yaml` / Compile / exportedModule).value
    ),
    Test / parallelExecution := false
  )
  .dependsOn(`scala-yaml`)

lazy val `task-progress-notifications` = project
  .in(file("lib/scala/task-progress-notifications"))
  .enablePlugins(JPMSPlugin)
  .configs(Test)
  .settings(
    scalaModuleDependencySetting,
    version := "0.1",
    compileOrder := CompileOrder.ScalaThenJava,
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % scalatestVersion % Test
    ),
    Compile / internalModuleDependencies := Seq(
      (`cli` / Compile / exportedModule).value,
      (`json-rpc-server` / Compile / exportedModule).value,
      (`akka-wrapper` / Compile / exportedModuleBin).value,
      (`scala-libs-wrapper` / Compile / exportedModule).value
    ),
    Test / parallelExecution := false
  )
  .dependsOn(cli)
  .dependsOn(`json-rpc-server`)

lazy val `version-output` = (project in file("lib/scala/version-output"))
  .enablePlugins(JPMSPlugin)
  .settings(
    version := "0.1"
  )
  .settings(
    frgaalJavaCompilerSetting,
    Compile / sourceGenerators += Def.task {
      val file =
        (Compile / sourceManaged).value / "org" / "enso" / "version" / "GeneratedVersion.java"
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
  .enablePlugins(JPMSPlugin)
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    scalaModuleDependencySetting,
    mixedJavaScalaProjectSetting,
    commands += WithDebugCommand.withDebug,
    version := "0.1",
    libraryDependencies ++= Seq(
      "junit"          % "junit"           % junitVersion   % Test,
      "com.github.sbt" % "junit-interface" % junitIfVersion % Test
    ),
    Compile / internalModuleDependencies := Seq(
      (`text-buffer` / Compile / exportedModule).value,
      (`runtime-parser` / Compile / exportedModule).value
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
    libraryDependencies ++= akka ++ Seq(akkaSLF4J, akkaTestkit % Test),
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
        streams.value.log,
        scalaBinaryVersion.value,
        moduleName.value
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
      (`syntax-rust-definition` / javaModuleName).value,
      (`profiling-utils` / javaModuleName).value,
      (`ydoc-server` / javaModuleName).value
    ),
    Test / moduleDependencies := {
      GraalVM.modules ++ GraalVM.langsPkgs ++ logbackPkg ++ helidon ++ Seq(
        "org.slf4j"        % "slf4j-api"                    % slf4jVersion,
        "org.netbeans.api" % "org-netbeans-modules-sampler" % netbeansApiVersion
      )
    },
    Test / internalModuleDependencies := Seq(
      (`profiling-utils` / Compile / exportedModule).value,
      (`syntax-rust-definition` / Compile / exportedModule).value,
      (`ydoc-server` / Compile / exportedModule).value
    ),
    Test / javaOptions ++= testLogProviderOptions,
    Test / test := (Test / test).dependsOn(buildEngineDistribution).value
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
  .dependsOn(`ydoc-server` % Test)
  .dependsOn(`profiling-utils` % Test)

lazy val `json-rpc-server` = project
  .in(file("lib/scala/json-rpc-server"))
  .enablePlugins(JPMSPlugin)
  .settings(
    frgaalJavaCompilerSetting,
    scalaModuleDependencySetting,
    compileOrder := CompileOrder.ScalaThenJava,
    libraryDependencies ++= akka ++ logbackTest,
    libraryDependencies ++= circe,
    libraryDependencies ++= Seq(
      "io.circe"                   %% "circe-literal"   % circeVersion,
      "com.typesafe.scala-logging" %% "scala-logging"   % scalaLoggingVersion,
      "org.slf4j"                   % "slf4j-api"       % slf4jVersion,
      akkaTestkit                   % Test,
      "org.scalatest"              %% "scalatest"       % scalatestVersion      % Test,
      "junit"                       % "junit"           % junitVersion          % Test,
      "com.github.sbt"              % "junit-interface" % junitIfVersion        % Test,
      "org.apache.httpcomponents"   % "httpclient"      % httpComponentsVersion % Test,
      "org.apache.httpcomponents"   % "httpcore"        % httpComponentsVersion % Test,
      "commons-io"                  % "commons-io"      % commonsIoVersion      % Test
    ),
    Compile / moduleDependencies ++=
      Seq(
        "org.slf4j" % "slf4j-api" % slf4jVersion
      ),
    Compile / internalModuleDependencies := Seq(
      (`scala-libs-wrapper` / Compile / exportedModule).value,
      (`akka-wrapper` / Compile / exportedModule).value
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

// An automatic JPMS module
lazy val testkit = project
  .in(file("lib/scala/testkit"))
  .enablePlugins(JPMSPlugin)
  .settings(
    frgaalJavaCompilerSetting,
    compileOrder := CompileOrder.ScalaThenJava,
    javaModuleName := "org.enso.testkit",
    libraryDependencies ++= logbackPkg ++ Seq(
      "org.apache.commons" % "commons-lang3"   % commonsLangVersion,
      "commons-io"         % "commons-io"      % commonsIoVersion,
      "org.scalatest"     %% "scalatest"       % scalatestVersion,
      "junit"              % "junit"           % junitVersion,
      "com.github.sbt"     % "junit-interface" % junitIfVersion,
      "org.slf4j"          % "slf4j-api"       % slf4jVersion
    ),
    packageOptions := Seq(
      Package.ManifestAttributes(
        (
          "Automatic-Module-Name",
          javaModuleName.value
        )
      )
    ),
    Compile / exportedModule := (Compile / exportedModuleBin).value,
    Compile / exportedModuleBin := (Compile / packageBin).value
  )
  .dependsOn(`logging-service-logback`)

lazy val searcher = project
  .in(file("lib/scala/searcher"))
  .enablePlugins(JPMSPlugin)
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    scalaModuleDependencySetting,
    compileOrder := CompileOrder.ScalaThenJava,
    annotationProcSetting,
    libraryDependencies ++= jmh ++ Seq(
      "org.scalatest" %% "scalatest" % scalatestVersion % Test
    ) ++ logbackTest,
    Compile / internalModuleDependencies := Seq(
      (`polyglot-api` / Compile / exportedModule).value
    )
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
    Compile / moduleDependencies ++= {
      GraalVM.modules ++ GraalVM.jsPkgs ++ GraalVM.chromeInspectorPkgs ++ helidon ++ logbackPkg ++ Seq(
        "org.slf4j" % "slf4j-api" % slf4jVersion
      )
    },
    Compile / internalModuleDependencies := Seq(
      (`syntax-rust-definition` / Compile / exportedModule).value,
      (`profiling-utils` / Compile / exportedModule).value
    ),
    libraryDependencies ++= Seq(
      "org.graalvm.truffle"        % "truffle-api"                 % graalMavenPackagesVersion % "provided",
      "org.graalvm.polyglot"       % "inspect"                     % graalMavenPackagesVersion % "runtime",
      "org.graalvm.polyglot"       % "js"                          % graalMavenPackagesVersion % "runtime",
      "org.slf4j"                  % "slf4j-api"                   % slf4jVersion,
      "io.helidon.webclient"       % "helidon-webclient-websocket" % helidonVersion,
      "io.helidon.webserver"       % "helidon-webserver-websocket" % helidonVersion,
      "junit"                      % "junit"                       % junitVersion              % Test,
      "com.github.sbt"             % "junit-interface"             % junitIfVersion            % Test,
      "com.fasterxml.jackson.core" % "jackson-databind"            % jacksonVersion            % Test
    ),
    libraryDependencies ++= {
      GraalVM.modules ++ GraalVM.jsPkgs ++ GraalVM.chromeInspectorPkgs ++ helidon
    }
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
        (Compile / modulePath).value ++ (`profiling-utils` / Compile / modulePath).value
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
  .enablePlugins(JPMSPlugin)
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
    ),
    Compile / moduleDependencies ++= Seq(
      "org.slf4j"        % "slf4j-api"               % slf4jVersion,
      "org.netbeans.api" % "org-openide-util-lookup" % netbeansApiVersion
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
  .enablePlugins(JPMSPlugin)
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
    ),
    Compile / moduleDependencies ++= Seq(
      "org.apache.commons" % "commons-lang3"           % commonsLangVersion,
      "org.netbeans.api"   % "org-openide-util-lookup" % netbeansApiVersion,
      "com.google.guava"   % "guava"                   % guavaVersion
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
  "-Dslf4j.provider=org.enso.logging.service.logback.test.provider.TestLogProvider",
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
  .enablePlugins(JPMSPlugin)
  .settings(
    frgaalJavaCompilerSetting,
    Test / fork := true,
    commands += WithDebugCommand.withDebug,
    Test / envVars ++= distributionEnvironmentOverrides,
    libraryDependencies ++= Seq(
      "org.graalvm.polyglot" % "polyglot" % graalMavenPackagesVersion % "provided"
    ),
    Compile / moduleDependencies ++= {
      Seq(
        "org.graalvm.polyglot" % "polyglot"  % graalMavenPackagesVersion,
        "org.slf4j"            % "slf4j-api" % slf4jVersion
      )
    },
    Compile / internalModuleDependencies := Seq(
      (`logging-utils` / Compile / exportedModule).value,
      (`logging-config` / Compile / exportedModule).value
    )
  )
  .dependsOn(`logging-config`)
  .dependsOn(`logging-utils`)
  .dependsOn(testkit % Test)

lazy val `polyglot-api` = project
  .in(file("engine/polyglot-api"))
  .enablePlugins(JPMSPlugin)
  .settings(
    frgaalJavaCompilerSetting,
    scalaModuleDependencySetting,
    mixedJavaScalaProjectSetting,
    Test / fork := true,
    commands += WithDebugCommand.withDebug,
    Test / envVars ++= distributionEnvironmentOverrides,
    Test / javaOptions ++= Seq(
      "-Dpolyglot.engine.WarnInterpreterOnly=false",
      "-Dpolyglotimpl.DisableClassPathIsolation=true"
    ),
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
    Compile / moduleDependencies ++= Seq(
      "com.google.flatbuffers" % "flatbuffers-java" % flatbuffersVersion,
      "org.graalvm.sdk"        % "word"             % graalMavenPackagesVersion,
      "org.graalvm.polyglot"   % "polyglot"         % graalMavenPackagesVersion,
      "org.graalvm.sdk"        % "collections"      % graalMavenPackagesVersion,
      "org.graalvm.sdk"        % "nativeimage"      % graalMavenPackagesVersion,
      "org.graalvm.truffle"    % "truffle-api"      % graalMavenPackagesVersion
    ),
    Compile / internalModuleDependencies := Seq(
      (`scala-libs-wrapper` / Compile / exportedModule).value,
      (`engine-common` / Compile / exportedModule).value,
      (`editions` / Compile / exportedModule).value,
      (`pkg` / Compile / exportedModule).value,
      (`logging-utils` / Compile / exportedModule).value,
      (`text-buffer` / Compile / exportedModule).value,
      (`polyglot-api-macros` / Compile / exportedModule).value
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
  .enablePlugins(JPMSPlugin)
  .settings(
    frgaalJavaCompilerSetting,
    scalaModuleDependencySetting,
    mixedJavaScalaProjectSetting,
    libraryDependencies ++= Seq(
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core"   % jsoniterVersion % "provided",
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % jsoniterVersion % "provided"
    ),
    Compile / internalModuleDependencies := Seq(
      (`scala-libs-wrapper` / Compile / exportedModule).value
    )
  )

lazy val `language-server` = (project in file("engine/language-server"))
  .enablePlugins(JPMSPlugin)
  .enablePlugins(PackageListPlugin)
  .settings(
    commands += WithDebugCommand.withDebug,
    frgaalJavaCompilerSetting,
    scalaModuleDependencySetting,
    mixedJavaScalaProjectSetting,
    libraryDependencies ++= akka ++ circe ++ bouncyCastle.map(_ % Test) ++ Seq(
      "org.slf4j"                   % "slf4j-api"            % slf4jVersion,
      "com.typesafe.scala-logging" %% "scala-logging"        % scalaLoggingVersion,
      "io.circe"                   %% "circe-generic-extras" % circeGenericExtrasVersion,
      "io.circe"                   %% "circe-literal"        % circeVersion,
      "dev.zio"                    %% "zio"                  % zioVersion,
      "com.google.flatbuffers"      % "flatbuffers-java"     % flatbuffersVersion,
      "commons-io"                  % "commons-io"           % commonsIoVersion,
      "com.github.pureconfig"      %% "pureconfig"           % pureconfigVersion,
      akkaSLF4J,
      akkaTestkit           % Test,
      "com.typesafe.akka"  %% "akka-http-testkit"       % akkaHTTPVersion           % Test,
      "org.scalatest"      %% "scalatest"               % scalatestVersion          % Test,
      "org.scalacheck"     %% "scalacheck"              % scalacheckVersion         % Test,
      "org.graalvm.truffle" % "truffle-api"             % graalMavenPackagesVersion % "provided",
      "org.graalvm.sdk"     % "polyglot-tck"            % graalMavenPackagesVersion % "provided",
      "org.netbeans.api"    % "org-openide-util-lookup" % netbeansApiVersion        % "provided",
      "org.eclipse.jgit"    % "org.eclipse.jgit"        % jgitVersion,
      "org.apache.tika"     % "tika-core"               % tikaVersion               % Test
    ),
    javaModuleName := "org.enso.language.server",
    Compile / moduleDependencies ++=
      Seq(
        "org.graalvm.polyglot"   % "polyglot"         % graalMavenPackagesVersion,
        "org.slf4j"              % "slf4j-api"        % slf4jVersion,
        "commons-cli"            % "commons-cli"      % commonsCliVersion,
        "commons-io"             % "commons-io"       % commonsIoVersion,
        "com.google.flatbuffers" % "flatbuffers-java" % flatbuffersVersion,
        "org.eclipse.jgit"       % "org.eclipse.jgit" % jgitVersion
      ),
    Compile / internalModuleDependencies := Seq(
      (`akka-wrapper` / Compile / exportedModule).value,
      (`zio-wrapper` / Compile / exportedModule).value,
      (`scala-libs-wrapper` / Compile / exportedModule).value,
      (`connected-lock-manager-server` / Compile / exportedModule).value,
      (`language-server-deps-wrapper` / Compile / exportedModule).value,
      (`directory-watcher-wrapper` / Compile / exportedModule).value,
      (`engine-runner-common` / Compile / exportedModule).value,
      (`ydoc-server` / Compile / exportedModule).value,
      (`logging-utils` / Compile / exportedModule).value,
      (`logging-utils-akka` / Compile / exportedModule).value,
      (`logging-service` / Compile / exportedModule).value,
      (`engine-common` / Compile / exportedModule).value,
      (`library-manager` / Compile / exportedModule).value,
      (`polyglot-api` / Compile / exportedModule).value,
      (`json-rpc-server` / Compile / exportedModule).value,
      (`profiling-utils` / Compile / exportedModule).value,
      (`searcher` / Compile / exportedModule).value,
      (`pkg` / Compile / exportedModule).value,
      (`distribution-manager` / Compile / exportedModule).value,
      (`edition-updater` / Compile / exportedModule).value,
      (`editions` / Compile / exportedModule).value,
      (`text-buffer` / Compile / exportedModule).value,
      (`filewatcher` / Compile / exportedModule).value,
      (`version-output` / Compile / exportedModule).value,
      (`semver` / Compile / exportedModule).value,
      (`cli` / Compile / exportedModule).value,
      (`task-progress-notifications` / Compile / exportedModule).value
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
    // More dependencies needed for modules for testing
    libraryDependencies ++= ioSentry.map(_ % Test) ++ logbackTest ++ Seq(
      "com.google.protobuf"    % "protobuf-java"                % googleProtobufVersion  % Test,
      "org.reactivestreams"    % "reactive-streams"             % reactiveStreamsVersion % Test,
      "org.jline"              % "jline"                        % jlineVersion           % Test,
      "org.apache.tika"        % "tika-core"                    % tikaVersion            % Test,
      "com.google.flatbuffers" % "flatbuffers-java"             % flatbuffersVersion     % Test,
      "org.netbeans.api"       % "org-netbeans-modules-sampler" % netbeansApiVersion     % Test,
      "org.apache.commons"     % "commons-lang3"                % commonsLangVersion     % Test,
      "org.apache.commons"     % "commons-compress"             % commonsCompressVersion % Test,
      "org.yaml"               % "snakeyaml"                    % snakeyamlVersion       % Test,
      "com.ibm.icu"            % "icu4j"                        % icuVersion             % Test
    ),
    Test / moduleDependencies := {
      GraalVM.modules ++ GraalVM.langsPkgs ++ logbackPkg ++ helidon ++ ioSentry ++ bouncyCastle ++ scalaLibrary ++ scalaCompiler ++ Seq(
        "org.slf4j"              % "slf4j-api"                    % slf4jVersion,
        "org.netbeans.api"       % "org-netbeans-modules-sampler" % netbeansApiVersion,
        "com.google.flatbuffers" % "flatbuffers-java"             % flatbuffersVersion,
        "org.yaml"               % "snakeyaml"                    % snakeyamlVersion,
        "com.typesafe"           % "config"                       % typesafeConfigVersion,
        "org.apache.commons"     % "commons-lang3"                % commonsLangVersion,
        "org.apache.commons"     % "commons-compress"             % commonsCompressVersion,
        "commons-io"             % "commons-io"                   % commonsIoVersion,
        "com.google.protobuf"    % "protobuf-java"                % googleProtobufVersion,
        "org.reactivestreams"    % "reactive-streams"             % reactiveStreamsVersion,
        "org.jline"              % "jline"                        % jlineVersion,
        "org.apache.tika"        % "tika-core"                    % tikaVersion,
        "com.ibm.icu"            % "icu4j"                        % icuVersion,
        "org.netbeans.api"       % "org-openide-util-lookup"      % netbeansApiVersion
      )
    },
    Test / internalModuleDependencies := Seq(
      (Compile / exportedModule).value,
      (`runtime` / Compile / exportedModule).value,
      (`runtime-instrument-common` / Compile / exportedModule).value,
      (`runtime-instrument-runtime-server` / Compile / exportedModule).value,
      (`runtime-instrument-repl-debugger` / Compile / exportedModule).value,
      (`runtime-instrument-id-execution` / Compile / exportedModule).value,
      (`runtime-language-epb` / Compile / exportedModule).value,
      (`ydoc-server` / Compile / exportedModule).value,
      (`syntax-rust-definition` / Compile / exportedModule).value,
      (`profiling-utils` / Compile / exportedModule).value,
      (`logging-service-logback` / Compile / exportedModule).value,
      (`logging-service-logback` / Test / exportedModule).value,
      (`version-output` / Compile / exportedModule).value,
      (`scala-libs-wrapper` / Compile / exportedModule).value,
      (`jna-wrapper` / Compile / exportedModule).value,
      (`akka-wrapper` / Compile / exportedModule).value,
      (`language-server-deps-wrapper` / Compile / exportedModule).value,
      (`fansi-wrapper` / Compile / exportedModule).value,
      (`text-buffer` / Compile / exportedModule).value,
      (`runtime-suggestions` / Compile / exportedModule).value,
      (`runtime-parser` / Compile / exportedModule).value,
      (`runtime-compiler` / Compile / exportedModule).value,
      (`polyglot-api` / Compile / exportedModule).value,
      (`polyglot-api-macros` / Compile / exportedModule).value,
      (`pkg` / Compile / exportedModule).value,
      (`logging-utils` / Compile / exportedModule).value,
      (`connected-lock-manager` / Compile / exportedModule).value,
      (`library-manager` / Compile / exportedModule).value,
      (`persistance` / Compile / exportedModule).value,
      (`interpreter-dsl` / Compile / exportedModule).value,
      (`engine-common` / Compile / exportedModule).value,
      (`edition-updater` / Compile / exportedModule).value,
      (`editions` / Compile / exportedModule).value,
      (`distribution-manager` / Compile / exportedModule).value,
      (`common-polyglot-core-utils` / Compile / exportedModule).value,
      (`cli` / Compile / exportedModule).value,
      (`refactoring-utils` / Compile / exportedModule).value,
      (`scala-yaml` / Compile / exportedModule).value,
      (`semver` / Compile / exportedModule).value,
      (`downloader` / Compile / exportedModule).value,
      (`logging-config` / Compile / exportedModule).value,
      (`logging-service` / Compile / exportedModule).value,
      (`task-progress-notifications` / Compile / exportedModule).value
    ),
    Test / javaOptions ++= testLogProviderOptions,
    Test / patchModules := {
      // Patch test-classes into the runtime module. This is standard way to deal with the
      // split package problem in unit tests. For example, Maven's surefire plugin does this.
      val testClassesDir = (Test / productDirectories).value.head
      // Patching with sources is useful for compilation, patching with compiled classes for runtime.
      val javaSrcDir = (Test / javaSource).value
      Map(
        javaModuleName.value -> Seq(javaSrcDir, testClassesDir)
      )
    },
    Test / addModules := Seq(
      javaModuleName.value,
      (`syntax-rust-definition` / javaModuleName).value,
      (`profiling-utils` / javaModuleName).value,
      (`ydoc-server` / javaModuleName).value,
      (`library-manager` / javaModuleName).value
    ),
    Test / addReads := {
      // We patched the test-classes into the runtime module. These classes access some stuff from
      // unnamed module. Thus, let's add ALL-UNNAMED.
      Map(
        javaModuleName.value -> Seq(
          "ALL-UNNAMED",
          "org.bouncycastle.provider"
        )
      )
    },
    Test / addExports := {
      val profModName       = (`profiling-utils` / javaModuleName).value
      val downloaderModName = (`downloader` / javaModuleName).value
      val exports = Map(
        profModName + "/org.enso.profiling.snapshot"       -> Seq("ALL-UNNAMED"),
        downloaderModName + "/org.enso.downloader.archive" -> Seq("ALL-UNNAMED")
      )

      // Make sure that all the packages in test source directory are exported
      // to all unnamed modules
      val testPkgs = (Test / packages).value
      val testPkgsExports = testPkgs.map { pkg =>
        javaModuleName.value + "/" + pkg -> Seq("ALL-UNNAMED")
      }.toMap
      exports ++ testPkgsExports
    }
  )
  .settings(
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
lazy val frgaalJavaCompilerSetting: SettingsDefinition =
  customFrgaalJavaCompilerSettings(targetJavaVersion)

lazy val scalaModuleDependencySetting: SettingsDefinition = Seq(
  Compile / moduleDependencies := scalaLibrary
)

lazy val mixedJavaScalaProjectSetting: SettingsDefinition = Seq(
  // See JPMSPlugin docs (Mixed projects)
  excludeFilter := excludeFilter.value || "module-info.java"
)

def customFrgaalJavaCompilerSettings(targetJdk: String) = {
  // There might be slightly different Frgaal compiler configuration for
  // both Compile and Test configurations
  Seq(Compile, Test).flatMap { config =>
    Seq(
      config / compile / compilers := {
        // True if there is module-info.java in the sources, and this is a mixed
        // project, and module-info.java is excluded from the compilation.
        // shouldCompileModuleInfoManually is a settingKey defined only in projects
        // with JPMSPlugin. That's why we have to check first for its existance.
        val settingOpt               = (config / shouldCompileModuleInfoManually).?.value
        val shouldCompileModInfo     = settingOpt.isDefined && settingOpt.get
        val shouldNotLimitModulesOpt = frgaalShouldNotLimitModules.?.value
        val _shouldNotLimitModules   = shouldNotLimitModulesOpt.getOrElse(false)
        val projName                 = projectID.value.name
        FrgaalJavaCompiler.compilers(
          (config / dependencyClasspath).value,
          compilers.value,
          targetJdk,
          shouldCompileModInfo,
          (config / javaSource).value,
          _shouldNotLimitModules
        )
      }
    )
  } ++ Seq(
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
}

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
    .enablePlugins(JPMSPlugin)
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
      ),
      Compile / moduleDependencies ++= Seq(
        "org.graalvm.truffle"  % "truffle-api" % graalMavenPackagesVersion,
        "org.graalvm.polyglot" % "polyglot"    % graalMavenPackagesVersion,
        "org.graalvm.sdk"      % "collections" % graalMavenPackagesVersion,
        "org.graalvm.sdk"      % "word"        % graalMavenPackagesVersion,
        "org.graalvm.sdk"      % "nativeimage" % graalMavenPackagesVersion
      )
    )

lazy val `runtime-language-arrow` =
  (project in file("engine/runtime-language-arrow"))
    .enablePlugins(JPMSPlugin)
    .settings(
      crossPaths := false,
      autoScalaLibrary := false,
      javaModuleName := "org.enso.interpreter.arrow",
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
      Compile / moduleDependencies ++= GraalVM.modules,
      Test / moduleDependencies += projectID.value,
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
      Compile / moduleDependencies ++= {
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
    scalaModuleDependencySetting,
    mixedJavaScalaProjectSetting,
    annotationProcSetting,
    truffleDslSuppressWarnsSetting,
    version := ensoVersion,
    commands += WithDebugCommand.withDebug,
    inConfig(Compile)(truffleRunOptionsSettings),
    libraryDependencies ++= Seq(
      "org.apache.commons"   % "commons-lang3"           % commonsLangVersion,
      "org.apache.tika"      % "tika-core"               % tikaVersion,
      "com.lihaoyi"         %% "fansi"                   % fansiVersion,
      "org.graalvm.polyglot" % "polyglot"                % graalMavenPackagesVersion % "provided",
      "org.graalvm.sdk"      % "polyglot-tck"            % graalMavenPackagesVersion % "provided",
      "org.graalvm.truffle"  % "truffle-api"             % graalMavenPackagesVersion % "provided",
      "org.graalvm.truffle"  % "truffle-dsl-processor"   % graalMavenPackagesVersion % "provided",
      "org.graalvm.regex"    % "regex"                   % graalMavenPackagesVersion % "provided",
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
    },
    javaModuleName := "org.enso.runtime",
    Compile / moduleDependencies ++= Seq(
      "org.netbeans.api"     % "org-openide-util-lookup" % netbeansApiVersion,
      "org.apache.tika"      % "tika-core"               % tikaVersion,
      "org.slf4j"            % "slf4j-api"               % slf4jVersion,
      "org.graalvm.truffle"  % "truffle-api"             % graalMavenPackagesVersion,
      "org.graalvm.polyglot" % "polyglot"                % graalMavenPackagesVersion,
      "org.graalvm.sdk"      % "collections"             % graalMavenPackagesVersion,
      "org.graalvm.sdk"      % "word"                    % graalMavenPackagesVersion,
      "org.graalvm.sdk"      % "nativeimage"             % graalMavenPackagesVersion,
      "com.ibm.icu"          % "icu4j"                   % icuVersion,
      "org.apache.commons"   % "commons-lang3"           % commonsLangVersion
    ),
    Compile / internalModuleDependencies := Seq(
      (`distribution-manager` / Compile / exportedModule).value,
      (`engine-common` / Compile / exportedModule).value,
      (`library-manager` / Compile / exportedModule).value,
      (`connected-lock-manager` / Compile / exportedModule).value,
      (`logging-utils` / Compile / exportedModule).value,
      (`runtime-compiler` / Compile / exportedModule).value,
      (`runtime-parser` / Compile / exportedModule).value,
      (`runtime-suggestions` / Compile / exportedModule).value,
      (`polyglot-api` / Compile / exportedModule).value,
      (`common-polyglot-core-utils` / Compile / exportedModule).value,
      (`pkg` / Compile / exportedModule).value,
      (`cli` / Compile / exportedModule).value,
      (`editions` / Compile / exportedModule).value,
      (`edition-updater` / Compile / exportedModule).value,
      (`syntax-rust-definition` / Compile / exportedModule).value,
      (`version-output` / Compile / exportedModule).value,
      (`interpreter-dsl` / Compile / exportedModule).value,
      (`persistance` / Compile / exportedModule).value,
      (`text-buffer` / Compile / exportedModule).value,
      (`scala-libs-wrapper` / Compile / exportedModule).value,
      (`fansi-wrapper` / Compile / exportedModule).value
    )
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

lazy val `runtime-and-langs` = (project in file("engine/runtime-and-langs"))
  .settings(
    libraryDependencies ++= {
      GraalVM.modules ++ GraalVM.langsPkgs
    }
  )
  .dependsOn(runtime)

/** A project holding all the runtime integration tests. These tests require, among other things,
  * the `org.enso.runtime` JPMS module, so it is easier to keep them in a separate project.
  * For standard unit tests, use `runtime/Test`.
  */
lazy val `runtime-integration-tests` =
  (project in file("engine/runtime-integration-tests"))
    .enablePlugins(JPMSPlugin)
    .enablePlugins(PackageListPlugin)
    .settings(
      frgaalJavaCompilerSetting,
      annotationProcSetting,
      commands += WithDebugCommand.withDebug,
      libraryDependencies ++= GraalVM.modules ++ GraalVM.langsPkgs ++ GraalVM.insightPkgs ++ logbackPkg ++ helidon ++ Seq(
        "org.graalvm.polyglot" % "polyglot"                     % graalMavenPackagesVersion % "provided",
        "org.graalvm.sdk"      % "polyglot-tck"                 % graalMavenPackagesVersion % "provided",
        "org.graalvm.truffle"  % "truffle-api"                  % graalMavenPackagesVersion % "provided",
        "org.graalvm.truffle"  % "truffle-dsl-processor"        % graalMavenPackagesVersion % "provided",
        "org.graalvm.truffle"  % "truffle-tck"                  % graalMavenPackagesVersion,
        "org.graalvm.truffle"  % "truffle-tck-common"           % graalMavenPackagesVersion,
        "org.graalvm.truffle"  % "truffle-tck-tests"            % graalMavenPackagesVersion,
        "org.netbeans.api"     % "org-openide-util-lookup"      % netbeansApiVersion,
        "org.netbeans.api"     % "org-netbeans-modules-sampler" % netbeansApiVersion,
        "org.scalacheck"      %% "scalacheck"                   % scalacheckVersion         % Test,
        "org.scalactic"       %% "scalactic"                    % scalacticVersion          % Test,
        "org.scalatest"       %% "scalatest"                    % scalatestVersion          % Test,
        "junit"                % "junit"                        % junitVersion              % Test,
        "com.github.sbt"       % "junit-interface"              % junitIfVersion            % Test,
        "org.hamcrest"         % "hamcrest-all"                 % hamcrestVersion           % Test,
        "org.yaml"             % "snakeyaml"                    % snakeyamlVersion,
        "org.slf4j"            % "slf4j-api"                    % slf4jVersion
      ),
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
      Test / moduleDependencies := {
        GraalVM.modules ++ GraalVM.langsPkgs ++ GraalVM.insightPkgs ++ logbackPkg ++ helidon ++ ioSentry ++ scalaLibrary ++ scalaCompiler ++ Seq(
          "org.apache.commons"     % "commons-lang3"                % commonsLangVersion,
          "org.apache.commons"     % "commons-compress"             % commonsCompressVersion,
          "commons-io"             % "commons-io"                   % commonsIoVersion,
          "org.apache.tika"        % "tika-core"                    % tikaVersion,
          "org.slf4j"              % "slf4j-api"                    % slf4jVersion,
          "org.netbeans.api"       % "org-openide-util-lookup"      % netbeansApiVersion,
          "org.netbeans.api"       % "org-netbeans-modules-sampler" % netbeansApiVersion,
          "org.graalvm.sdk"        % "polyglot-tck"                 % graalMavenPackagesVersion,
          "org.graalvm.truffle"    % "truffle-tck"                  % graalMavenPackagesVersion,
          "org.graalvm.truffle"    % "truffle-tck-common"           % graalMavenPackagesVersion,
          "org.graalvm.truffle"    % "truffle-tck-tests"            % graalMavenPackagesVersion,
          "com.ibm.icu"            % "icu4j"                        % icuVersion,
          "org.jline"              % "jline"                        % jlineVersion,
          "com.google.flatbuffers" % "flatbuffers-java"             % flatbuffersVersion,
          "org.yaml"               % "snakeyaml"                    % snakeyamlVersion,
          "com.typesafe"           % "config"                       % typesafeConfigVersion
        )
      },
      Test / internalModuleDependencies := Seq(
        (`runtime` / Compile / exportedModule).value,
        (`runtime-test-instruments` / Compile / exportedModule).value,
        (`runtime-instrument-common` / Compile / exportedModule).value,
        (`runtime-instrument-runtime-server` / Compile / exportedModule).value,
        (`runtime-instrument-repl-debugger` / Compile / exportedModule).value,
        (`runtime-instrument-id-execution` / Compile / exportedModule).value,
        (`runtime-language-epb` / Compile / exportedModule).value,
        (`ydoc-server` / Compile / exportedModule).value,
        (`syntax-rust-definition` / Compile / exportedModule).value,
        (`profiling-utils` / Compile / exportedModule).value,
        (`logging-service-logback` / Compile / exportedModule).value,
        (`logging-service-logback` / Test / exportedModule).value,
        (`version-output` / Compile / exportedModule).value,
        (`scala-libs-wrapper` / Compile / exportedModule).value,
        (`fansi-wrapper` / Compile / exportedModule).value,
        (`text-buffer` / Compile / exportedModule).value,
        (`runtime-suggestions` / Compile / exportedModule).value,
        (`runtime-parser` / Compile / exportedModule).value,
        (`runtime-compiler` / Compile / exportedModule).value,
        (`polyglot-api` / Compile / exportedModule).value,
        (`polyglot-api-macros` / Compile / exportedModule).value,
        (`pkg` / Compile / exportedModule).value,
        (`logging-utils` / Compile / exportedModule).value,
        (`connected-lock-manager` / Compile / exportedModule).value,
        (`library-manager` / Compile / exportedModule).value,
        (`persistance` / Compile / exportedModule).value,
        (`interpreter-dsl` / Compile / exportedModule).value,
        (`engine-common` / Compile / exportedModule).value,
        (`edition-updater` / Compile / exportedModule).value,
        (`editions` / Compile / exportedModule).value,
        (`distribution-manager` / Compile / exportedModule).value,
        (`common-polyglot-core-utils` / Compile / exportedModule).value,
        (`cli` / Compile / exportedModule).value,
        (`refactoring-utils` / Compile / exportedModule).value,
        (`scala-yaml` / Compile / exportedModule).value,
        (`semver` / Compile / exportedModule).value,
        (`downloader` / Compile / exportedModule).value,
        (`logging-config` / Compile / exportedModule).value,
        (`logging-service` / Compile / exportedModule).value
      ),
      Test / patchModules := {
        // Patch test-classes into the runtime module. This is standard way to deal with the
        // split package problem in unit tests. For example, Maven's surefire plugin does this.
        val testClassesDir = (Test / productDirectories).value.head
        // Patching with sources is useful for compilation, patching with compiled classes for runtime.
        val javaSrcDir = (Test / javaSource).value
        //(`logging-service-logback`)
        Map(
          (`runtime` / javaModuleName).value -> Seq(javaSrcDir, testClassesDir)
        )
      },
      // runtime-integration-tests does not have module descriptor on its own, so we have
      // to explicitly add some modules to the resolution.
      Test / addModules := Seq(
        "scala.library",
        (`runtime` / javaModuleName).value,
        (`runtime-test-instruments` / javaModuleName).value,
        (`ydoc-server` / javaModuleName).value,
        (`runtime-instrument-common` / javaModuleName).value,
        (`text-buffer` / javaModuleName).value,
        "truffle.tck.tests"
      ),
      Test / addReads := {
        val runtimeModName = (`runtime` / javaModuleName).value
        val testInstrumentsModName =
          (`runtime-test-instruments` / javaModuleName).value
        Map(
          // We patched the test-classes into the runtime module. These classes access some stuff from
          // unnamed module. Thus, let's add ALL-UNNAMED.
          runtimeModName -> Seq(
            "ALL-UNNAMED",
            testInstrumentsModName,
            (`runtime-instrument-common` / javaModuleName).value,
            (`text-buffer` / javaModuleName).value,
            (`semver` / javaModuleName).value,
            "truffle.tck.tests",
            "org.openide.util.lookup.RELEASE180"
          ),
          testInstrumentsModName -> Seq(runtimeModName)
        )
      },
      Test / addExports := {
        val runtimeModName = (`runtime` / javaModuleName).value
        val exports = Map(
          (`runtime-instrument-common` / javaModuleName).value + "/org.enso.interpreter.instrument.job" -> Seq(
            (`runtime` / javaModuleName).value
          ),
          (`runtime` / javaModuleName).value + "/org.enso.compiler.test" -> Seq(
            "ALL-UNNAMED"
          )
        )
        // Make sure that all the packages in test source directory are exported
        // to all unnamed modules
        val testPkgs = (Test / packages).value
        val testPkgsExports = testPkgs.map { pkg =>
          runtimeModName + "/" + pkg -> Seq("ALL-UNNAMED")
        }.toMap
        exports ++ testPkgsExports
      }
    )
    .dependsOn(`runtime`)
    .dependsOn(`runtime-test-instruments`)
    .dependsOn(`logging-service-logback` % "test->test")
    .dependsOn(testkit % Test)
    .dependsOn(`connected-lock-manager-server`)
    .dependsOn(`test-utils`)

/** A project that holds only benchmarks for `runtime`.
  */
lazy val `runtime-benchmarks` =
  (project in file("engine/runtime-benchmarks"))
    .enablePlugins(JPMSPlugin)
    .enablePlugins(PackageListPlugin)
    .settings(
      frgaalJavaCompilerSetting,
      scalaModuleDependencySetting,
      annotationProcSetting,
      // Note that withDebug command only makes sense if you use `@Fork(0)` in your benchmarks.
      commands += WithDebugCommand.withDebug,
      libraryDependencies ++= GraalVM.modules ++ GraalVM.langsPkgs ++ GraalVM.toolsPkgs ++ helidon ++ ioSentry ++ logbackPkg ++ Seq(
        "org.openjdk.jmh"     % "jmh-core"                     % jmhVersion,
        "org.openjdk.jmh"     % "jmh-generator-annprocess"     % jmhVersion,
        "jakarta.xml.bind"    % "jakarta.xml.bind-api"         % jaxbVersion,
        "com.sun.xml.bind"    % "jaxb-impl"                    % jaxbVersion,
        "org.graalvm.truffle" % "truffle-api"                  % graalMavenPackagesVersion,
        "org.graalvm.truffle" % "truffle-dsl-processor"        % graalMavenPackagesVersion % "provided",
        "org.slf4j"           % "slf4j-api"                    % slf4jVersion,
        "org.slf4j"           % "slf4j-nop"                    % slf4jVersion,
        "org.netbeans.api"    % "org-netbeans-modules-sampler" % netbeansApiVersion
      ),
      mainClass :=
        Some("org.enso.interpreter.bench.benchmarks.RuntimeBenchmarksRunner"),
      javacOptions --= Seq(
        "-source",
        frgaalSourceLevel,
        "--enable-preview"
      ),
      parallelExecution := false,
      Compile / moduleDependencies ++= {
        GraalVM.modules ++ GraalVM.langsPkgs ++ GraalVM.insightPkgs ++ logbackPkg ++ helidon ++ ioSentry ++ scalaCompiler ++ Seq(
          "org.apache.commons"     % "commons-lang3"                % commonsLangVersion,
          "org.apache.commons"     % "commons-compress"             % commonsCompressVersion,
          "commons-io"             % "commons-io"                   % commonsIoVersion,
          "org.apache.tika"        % "tika-core"                    % tikaVersion,
          "org.slf4j"              % "slf4j-api"                    % slf4jVersion,
          "org.slf4j"              % "slf4j-nop"                    % slf4jVersion,
          "org.netbeans.api"       % "org-openide-util-lookup"      % netbeansApiVersion,
          "org.netbeans.api"       % "org-netbeans-modules-sampler" % netbeansApiVersion,
          "com.ibm.icu"            % "icu4j"                        % icuVersion,
          "org.jline"              % "jline"                        % jlineVersion,
          "com.google.flatbuffers" % "flatbuffers-java"             % flatbuffersVersion,
          "org.yaml"               % "snakeyaml"                    % snakeyamlVersion,
          "com.typesafe"           % "config"                       % typesafeConfigVersion,
          // Dependencies for benchmarks-common
          "org.openjdk.jmh"    % "jmh-core"               % jmhVersion, // Automatic module
          "jakarta.xml.bind"   % "jakarta.xml.bind-api"   % jaxbVersion,
          "jakarta.activation" % "jakarta.activation-api" % jaActivationVersion
        )
      },
      Compile / internalModuleDependencies := Seq(
        (`runtime` / Compile / exportedModule).value,
        (`runtime-instrument-common` / Compile / exportedModule).value,
        (`runtime-instrument-runtime-server` / Compile / exportedModule).value,
        (`runtime-instrument-repl-debugger` / Compile / exportedModule).value,
        (`runtime-instrument-id-execution` / Compile / exportedModule).value,
        (`runtime-language-epb` / Compile / exportedModule).value,
        (`runtime-language-arrow` / Compile / exportedModule).value,
        (`ydoc-server` / Compile / exportedModule).value,
        (`benchmarks-common` / Compile / exportedModule).value,
        (`syntax-rust-definition` / Compile / exportedModule).value,
        (`profiling-utils` / Compile / exportedModule).value,
        (`logging-service-logback` / Compile / exportedModule).value,
        (`logging-service-logback` / Test / exportedModule).value,
        (`version-output` / Compile / exportedModule).value,
        (`scala-libs-wrapper` / Compile / exportedModule).value,
        (`fansi-wrapper` / Compile / exportedModule).value,
        (`text-buffer` / Compile / exportedModule).value,
        (`runtime-suggestions` / Compile / exportedModule).value,
        (`runtime-parser` / Compile / exportedModule).value,
        (`runtime-compiler` / Compile / exportedModule).value,
        (`polyglot-api` / Compile / exportedModule).value,
        (`polyglot-api-macros` / Compile / exportedModule).value,
        (`pkg` / Compile / exportedModule).value,
        (`logging-utils` / Compile / exportedModule).value,
        (`connected-lock-manager` / Compile / exportedModule).value,
        (`library-manager` / Compile / exportedModule).value,
        (`persistance` / Compile / exportedModule).value,
        (`interpreter-dsl` / Compile / exportedModule).value,
        (`engine-common` / Compile / exportedModule).value,
        (`edition-updater` / Compile / exportedModule).value,
        (`editions` / Compile / exportedModule).value,
        (`distribution-manager` / Compile / exportedModule).value,
        (`common-polyglot-core-utils` / Compile / exportedModule).value,
        (`cli` / Compile / exportedModule).value,
        (`refactoring-utils` / Compile / exportedModule).value,
        (`scala-yaml` / Compile / exportedModule).value,
        (`semver` / Compile / exportedModule).value,
        (`downloader` / Compile / exportedModule).value,
        (`logging-config` / Compile / exportedModule).value,
        (`logging-service` / Compile / exportedModule).value
      ),
      Compile / addModules := Seq(
        (`runtime` / javaModuleName).value,
        (`benchmarks-common` / javaModuleName).value,
        "org.slf4j.nop"
      ),
      // Benchmark sources are patched into the `org.enso.runtime` module
      Compile / patchModules := {
        val runtimeModName      = (`runtime` / javaModuleName).value
        val javaSrcDir          = (Compile / javaSource).value
        val classesDir          = (Compile / productDirectories).value.head
        val generatedClassesDir = (Compile / sourceManaged).value
        val testUtilsClasses =
          (`test-utils` / Compile / productDirectories).value.head
        Map(
          runtimeModName -> Seq(
            javaSrcDir,
            classesDir,
            testUtilsClasses,
            generatedClassesDir
          )
        )
      },
      // jmh is in unnamed modules
      Compile / addReads := {
        val runtimeModName = (`runtime` / javaModuleName).value
        Map(
          runtimeModName -> Seq(
            "ALL-UNNAMED",
            (`benchmarks-common` / javaModuleName).value
          )
        )
      },
      Compile / addExports := {
        val runtimeModName = (`runtime` / javaModuleName).value
        val pkgs           = (Compile / packages).value
        val pkgsExports = pkgs.map { pkg =>
          runtimeModName + "/" + pkg -> Seq("ALL-UNNAMED")
        }.toMap

        pkgsExports ++ Map(
          "org.slf4j.nop/org.slf4j.nop" -> Seq("org.slf4j")
        )
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
    .dependsOn(`benchmarks-common`)
    .dependsOn(`test-utils`)
    .dependsOn(`runtime`)

lazy val `runtime-parser` =
  (project in file("engine/runtime-parser"))
    .enablePlugins(JPMSPlugin)
    .settings(
      scalaModuleDependencySetting,
      mixedJavaScalaProjectSetting,
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
      ),
      Compile / moduleDependencies ++= Seq(
        "org.netbeans.api" % "org-openide-util-lookup" % netbeansApiVersion
      ),
      Compile / internalModuleDependencies := Seq(
        (`syntax-rust-definition` / Compile / exportedModule).value,
        (`persistance` / Compile / exportedModule).value
      )
    )
    .dependsOn(`syntax-rust-definition`)
    .dependsOn(`persistance`)
    .dependsOn(`persistance-dsl` % "provided")

lazy val `runtime-compiler` =
  (project in file("engine/runtime-compiler"))
    .enablePlugins(JPMSPlugin)
    .enablePlugins(PackageListPlugin)
    .settings(
      frgaalJavaCompilerSetting,
      scalaModuleDependencySetting,
      mixedJavaScalaProjectSetting,
      annotationProcSetting,
      commands += WithDebugCommand.withDebug,
      javaModuleName := "org.enso.runtime.compiler",
      (Test / fork) := true,
      libraryDependencies ++= Seq(
        "junit"                % "junit"                   % junitVersion              % Test,
        "com.github.sbt"       % "junit-interface"         % junitIfVersion            % Test,
        "org.scalatest"       %% "scalatest"               % scalatestVersion          % Test,
        "org.netbeans.api"     % "org-openide-util-lookup" % netbeansApiVersion        % "provided",
        "org.yaml"             % "snakeyaml"               % snakeyamlVersion          % Test,
        "org.jline"            % "jline"                   % jlineVersion              % Test,
        "com.typesafe"         % "config"                  % typesafeConfigVersion     % Test,
        "org.graalvm.polyglot" % "polyglot"                % graalMavenPackagesVersion % Test
      ),
      Compile / moduleDependencies ++= Seq(
        "org.slf4j"        % "slf4j-api"               % slf4jVersion,
        "org.netbeans.api" % "org-openide-util-lookup" % netbeansApiVersion
      ),
      Compile / internalModuleDependencies := Seq(
        (`engine-common` / Compile / exportedModule).value,
        (`pkg` / Compile / exportedModule).value,
        (`runtime-parser` / Compile / exportedModule).value,
        (`syntax-rust-definition` / Compile / exportedModule).value,
        (`persistance` / Compile / exportedModule).value,
        (`editions` / Compile / exportedModule).value
      ),
      Test / moduleDependencies := {
        (Compile / moduleDependencies).value ++ scalaLibrary ++ scalaCompiler ++ Seq(
          "org.apache.commons"   % "commons-compress" % commonsCompressVersion,
          "org.yaml"             % "snakeyaml"        % snakeyamlVersion,
          "org.jline"            % "jline"            % jlineVersion,
          "com.typesafe"         % "config"           % typesafeConfigVersion,
          "org.graalvm.polyglot" % "polyglot"         % graalMavenPackagesVersion
        )
      },
      Test / internalModuleDependencies := {
        val compileDeps = (Compile / internalModuleDependencies).value
        compileDeps ++ Seq(
          (Compile / exportedModule).value,
          (`scala-libs-wrapper` / Compile / exportedModule).value,
          (`version-output` / Compile / exportedModule).value,
          (`scala-yaml` / Compile / exportedModule).value,
          (`logging-config` / Compile / exportedModule).value,
          (`logging-utils` / Compile / exportedModule).value,
          (`semver` / Compile / exportedModule).value
        )
      },
      Test / addModules := Seq(
        javaModuleName.value
      ),
      Test / patchModules := {
        val testClassDir = (Test / productDirectories).value.head
        Map(
          javaModuleName.value -> Seq(
            testClassDir
          )
        )
      },
      Test / addExports := {
        val modName  = javaModuleName.value
        val testPkgs = (Test / packages).value
        val testPkgsExports = testPkgs.map { pkg =>
          modName + "/" + pkg -> Seq("ALL-UNNAMED")
        }.toMap

        testPkgsExports
      },
      Test / addReads := {
        Map(javaModuleName.value -> Seq("ALL-UNNAMED"))
      }
    )
    .dependsOn(`runtime-parser`)
    .dependsOn(pkg)
    .dependsOn(`engine-common`)
    .dependsOn(editions)
    .dependsOn(`persistance-dsl` % "provided")

lazy val `runtime-suggestions` =
  (project in file("engine/runtime-suggestions"))
    .enablePlugins(JPMSPlugin)
    .settings(
      frgaalJavaCompilerSetting,
      scalaModuleDependencySetting,
      mixedJavaScalaProjectSetting,
      (Test / fork) := true,
      libraryDependencies ++= Seq(
        "junit"            % "junit"                   % junitVersion       % Test,
        "com.github.sbt"   % "junit-interface"         % junitIfVersion     % Test,
        "org.scalatest"   %% "scalatest"               % scalatestVersion   % Test,
        "org.netbeans.api" % "org-openide-util-lookup" % netbeansApiVersion % "provided"
      ),
      Compile / internalModuleDependencies := Seq(
        (`pkg` / Compile / exportedModule).value,
        (`polyglot-api` / Compile / exportedModule).value,
        (`runtime-compiler` / Compile / exportedModule).value,
        (`runtime-parser` / Compile / exportedModule).value,
        (`text-buffer` / Compile / exportedModule).value
      )
    )
    .dependsOn(`runtime-compiler`)
    .dependsOn(`polyglot-api`)

lazy val `runtime-instrument-common` =
  (project in file("engine/runtime-instrument-common"))
    .enablePlugins(JPMSPlugin)
    .configs(Benchmark)
    .settings(
      frgaalJavaCompilerSetting,
      scalaModuleDependencySetting,
      mixedJavaScalaProjectSetting,
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
      ),
      javaModuleName := "org.enso.runtime.instrument.common",
      Compile / moduleDependencies ++= Seq(
        "org.graalvm.truffle"  % "truffle-api" % graalMavenPackagesVersion,
        "org.graalvm.polyglot" % "polyglot"    % graalMavenPackagesVersion,
        "org.graalvm.sdk"      % "collections" % graalMavenPackagesVersion,
        "org.graalvm.sdk"      % "nativeimage" % graalMavenPackagesVersion,
        "org.graalvm.sdk"      % "word"        % graalMavenPackagesVersion,
        "org.slf4j"            % "slf4j-api"   % slf4jVersion
      ),
      Compile / internalModuleDependencies := Seq(
        (`cli` / Compile / exportedModule).value,
        (`distribution-manager` / Compile / exportedModule).value,
        (`connected-lock-manager` / Compile / exportedModule).value,
        (`logging-utils` / Compile / exportedModule).value,
        (`editions` / Compile / exportedModule).value,
        (`engine-common` / Compile / exportedModule).value,
        (`refactoring-utils` / Compile / exportedModule).value,
        (`runtime` / Compile / exportedModule).value,
        (`runtime-compiler` / Compile / exportedModule).value,
        (`runtime-parser` / Compile / exportedModule).value,
        (`runtime-suggestions` / Compile / exportedModule).value,
        (`text-buffer` / Compile / exportedModule).value,
        (`pkg` / Compile / exportedModule).value,
        (`polyglot-api` / Compile / exportedModule).value,
        (`scala-libs-wrapper` / Compile / exportedModule).value
      )
    )
    .dependsOn(`refactoring-utils`)
    .dependsOn(`runtime` % "compile->compile;runtime->runtime;bench->bench")

lazy val `runtime-instrument-id-execution` =
  (project in file("engine/runtime-instrument-id-execution"))
    .enablePlugins(JPMSPlugin)
    .settings(
      frgaalJavaCompilerSetting,
      inConfig(Compile)(truffleRunOptionsSettings),
      Compile / forceModuleInfoCompilation := true,
      instrumentationSettings,
      Compile / moduleDependencies ++= Seq(
        "org.graalvm.truffle"  % "truffle-api" % graalMavenPackagesVersion,
        "org.graalvm.polyglot" % "polyglot"    % graalMavenPackagesVersion,
        "org.graalvm.sdk"      % "collections" % graalMavenPackagesVersion,
        "org.graalvm.sdk"      % "word"        % graalMavenPackagesVersion,
        "org.graalvm.sdk"      % "nativeimage" % graalMavenPackagesVersion
      ),
      Compile / internalModuleDependencies := Seq(
        (`runtime` / Compile / exportedModule).value,
        (`runtime-compiler` / Compile / exportedModule).value,
        (`polyglot-api` / Compile / exportedModule).value
      )
    )
    .dependsOn(`runtime`)
    .dependsOn(`runtime-instrument-common`)

lazy val `runtime-instrument-repl-debugger` =
  (project in file("engine/runtime-instrument-repl-debugger"))
    .enablePlugins(JPMSPlugin)
    .settings(
      scalaModuleDependencySetting,
      inConfig(Compile)(truffleRunOptionsSettings),
      Compile / forceModuleInfoCompilation := true,
      instrumentationSettings,
      Compile / moduleDependencies ++= Seq(
        "org.graalvm.truffle"  % "truffle-api" % graalMavenPackagesVersion,
        "org.graalvm.polyglot" % "polyglot"    % graalMavenPackagesVersion,
        "org.graalvm.sdk"      % "collections" % graalMavenPackagesVersion,
        "org.graalvm.sdk"      % "word"        % graalMavenPackagesVersion,
        "org.graalvm.sdk"      % "nativeimage" % graalMavenPackagesVersion
      ),
      Compile / internalModuleDependencies := Seq(
        (`runtime-instrument-common` / Compile / exportedModule).value,
        (`engine-common` / Compile / exportedModule).value,
        (`runtime` / Compile / exportedModule).value,
        (`polyglot-api` / Compile / exportedModule).value,
        (`runtime-compiler` / Compile / exportedModule).value,
        (`runtime-parser` / Compile / exportedModule).value
      )
    )
    .dependsOn(`runtime`)
    .dependsOn(`runtime-instrument-common`)

lazy val `runtime-instrument-runtime-server` =
  (project in file("engine/runtime-instrument-runtime-server"))
    .enablePlugins(JPMSPlugin)
    .settings(
      inConfig(Compile)(truffleRunOptionsSettings),
      Compile / forceModuleInfoCompilation := true,
      instrumentationSettings,
      Compile / moduleDependencies ++= Seq(
        "org.graalvm.truffle"  % "truffle-api"             % graalMavenPackagesVersion,
        "org.graalvm.polyglot" % "polyglot"                % graalMavenPackagesVersion,
        "org.graalvm.sdk"      % "collections"             % graalMavenPackagesVersion,
        "org.graalvm.sdk"      % "word"                    % graalMavenPackagesVersion,
        "org.graalvm.sdk"      % "nativeimage"             % graalMavenPackagesVersion,
        "org.netbeans.api"     % "org-openide-util-lookup" % netbeansApiVersion
      ),
      Compile / internalModuleDependencies := Seq(
        (`runtime-instrument-common` / Compile / exportedModule).value,
        (`engine-common` / Compile / exportedModule).value,
        (`distribution-manager` / Compile / exportedModule).value,
        (`runtime` / Compile / exportedModule).value,
        (`polyglot-api` / Compile / exportedModule).value,
        (`runtime-compiler` / Compile / exportedModule).value,
        (`connected-lock-manager` / Compile / exportedModule).value
      )
    )
    .dependsOn(`runtime`)
    .dependsOn(`runtime-instrument-common` % "test->test;compile->compile")

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
  .enablePlugins(JPMSPlugin)
  .settings(
    frgaalJavaCompilerSetting,
    scalaModuleDependencySetting,
    compileOrder := CompileOrder.ScalaThenJava, // Note [JPMS Compile order]
    Test / fork := true,
    commands += WithDebugCommand.withDebug,
    Test / envVars ++= distributionEnvironmentOverrides,
    libraryDependencies ++= Seq(
      "org.graalvm.polyglot" % "polyglot"    % graalMavenPackagesVersion % "provided",
      "commons-io"           % "commons-io"  % commonsIoVersion,
      "commons-cli"          % "commons-cli" % commonsCliVersion
    ),
    Compile / moduleDependencies ++= Seq(
      "commons-cli" % "commons-cli" % commonsCliVersion,
      "org.slf4j"   % "slf4j-api"   % slf4jVersion,
      "commons-io"  % "commons-io"  % commonsIoVersion
    ),
    Compile / internalModuleDependencies := Seq(
      (`pkg` / Compile / exportedModule).value,
      (`polyglot-api` / Compile / exportedModule).value,
      (`editions` / Compile / exportedModule).value,
      (`engine-common` / Compile / exportedModule).value,
      (`library-manager` / Compile / exportedModule).value
    )
  )
  .dependsOn(`polyglot-api`)
  .dependsOn(`library-manager`)
  .dependsOn(`edition-updater`)
  .dependsOn(testkit % Test)

lazy val `engine-runner` = project
  .in(file("engine/runner"))
  .enablePlugins(JPMSPlugin)
  .settings(
    frgaalJavaCompilerSetting,
    scalaModuleDependencySetting,
    compileOrder := CompileOrder.JavaThenScala,
    truffleDslSuppressWarnsSetting,
    packageOptions := Seq(
      // The `Multi-Release: true` comes from the `org.xerial/sqlite-jdbc` dependency.
      // But the current version of sbt-assembly does not allow to merge MANIFEST.MF
      // files this way.
      Package.ManifestAttributes(("Multi-Release", "true"))
    ),
    Compile / run / mainClass := Some("org.enso.runner.Main"),
    commands += WithDebugCommand.withDebug,
    inConfig(Compile)(truffleRunOptionsSettings),
    libraryDependencies ++= GraalVM.modules ++ Seq(
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
    Compile / moduleDependencies ++=
      Seq(
        "org.graalvm.polyglot" % "polyglot"    % graalMavenPackagesVersion,
        "org.graalvm.sdk"      % "nativeimage" % graalMavenPackagesVersion,
        "org.graalvm.sdk"      % "word"        % graalMavenPackagesVersion,
        "commons-cli"          % "commons-cli" % commonsCliVersion,
        "org.jline"            % "jline"       % jlineVersion,
        "org.slf4j"            % "slf4j-api"   % slf4jVersion
      ),
    Compile / internalModuleDependencies := Seq(
      (`profiling-utils` / Compile / exportedModule).value,
      (`semver` / Compile / exportedModule).value,
      (`cli` / Compile / exportedModule).value,
      (`distribution-manager` / Compile / exportedModule).value,
      (`editions` / Compile / exportedModule).value,
      (`edition-updater` / Compile / exportedModule).value,
      (`library-manager` / Compile / exportedModule).value,
      (`pkg` / Compile / exportedModule).value,
      (`engine-runner-common` / Compile / exportedModule).value,
      (`runtime-parser` / Compile / exportedModule).value,
      (`version-output` / Compile / exportedModule).value,
      (`engine-common` / Compile / exportedModule).value,
      (`polyglot-api` / Compile / exportedModule).value,
      (`logging-config` / Compile / exportedModule).value,
      (`logging-utils` / Compile / exportedModule).value
    ),
    run / connectInput := true
  )
  .settings(
    NativeImage.smallJdk := Some(buildSmallJdk.value),
    NativeImage.additionalCp := {
      val runnerDeps =
        (Compile / fullClasspath).value.map(_.data.getAbsolutePath)
      val runtimeDeps =
        (`runtime` / Compile / fullClasspath).value.map(_.data.getAbsolutePath)
      val loggingDeps =
        (`logging-service-logback` / Compile / fullClasspath).value.map(
          _.data.getAbsolutePath
        )
      val replDebugInstr =
        (`runtime-instrument-repl-debugger` / Compile / fullClasspath).value
          .map(_.data.getAbsolutePath)
      val runtimeServerInstr =
        (`runtime-instrument-runtime-server` / Compile / fullClasspath).value
          .map(_.data.getAbsolutePath)
      val idExecInstr =
        (`runtime-instrument-id-execution` / Compile / fullClasspath).value
          .map(_.data.getAbsolutePath)
      val epbLang =
        (`runtime-language-epb` / Compile / fullClasspath).value
          .map(_.data.getAbsolutePath)
      val core = (
        runnerDeps ++
          runtimeDeps ++
          loggingDeps ++
          replDebugInstr ++
          runtimeServerInstr ++
          idExecInstr ++
          epbLang
      ).distinct
      val stdLibsJars =
        `base-polyglot-root`.listFiles("*.jar").map(_.getAbsolutePath()) ++
        `table-polyglot-root`.listFiles("*.jar").map(_.getAbsolutePath())
      core ++ stdLibsJars
    },
    buildSmallJdk := {
      val smallJdkDirectory = (target.value / "jdk").getAbsoluteFile()
      if (smallJdkDirectory.exists()) {
        IO.delete(smallJdkDirectory)
      }
      val NI_MODULES =
        "org.graalvm.nativeimage,org.graalvm.nativeimage.builder,org.graalvm.nativeimage.base,org.graalvm.nativeimage.driver,org.graalvm.nativeimage.librarysupport,org.graalvm.nativeimage.objectfile,org.graalvm.nativeimage.pointsto,com.oracle.graal.graal_enterprise,com.oracle.svm.svm_enterprise"
      val JDK_MODULES =
        "jdk.localedata,jdk.httpserver,java.naming,java.net.http"
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
              "org.apache",
              "org.openxmlformats",
              "org.jline",
              "io.methvin.watchservice",
              "zio.internal",
              "org.enso.runner",
              "sun.awt",
              "sun.java2d",
              "sun.font",
              "java.awt",
              "com.sun.imageio",
              "com.sun.jna",
              "com.microsoft",
              "akka.http",
              "org.enso.base",
              "org.enso.image",
              "org.enso.table"
            )
          )
      }
      .dependsOn(NativeImage.additionalCp)
      .dependsOn(NativeImage.smallJdk)
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
        mainClass = Some("org.enso.launcher.cli.Main")
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
  .enablePlugins(JPMSPlugin)
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    scalaModuleDependencySetting,
    // Note [JPMS Compile order]
    compileOrder := CompileOrder.ScalaThenJava,
    resolvers += Resolver.bintrayRepo("gn0s1s", "releases"),
    libraryDependencies ++= Seq(
      "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion,
      "org.yaml"                    % "snakeyaml"     % snakeyamlVersion,
      "commons-io"                  % "commons-io"    % commonsIoVersion,
      "org.scalatest"              %% "scalatest"     % scalatestVersion % Test
    ),
    Compile / moduleDependencies ++= Seq(
      "org.slf4j" % "slf4j-api" % slf4jVersion,
      "org.yaml"  % "snakeyaml" % snakeyamlVersion
    ),
    Compile / internalModuleDependencies := Seq(
      (`cli` / Compile / exportedModule).value,
      (`logging-utils` / Compile / exportedModule).value,
      (`scala-libs-wrapper` / Compile / exportedModule).value,
      (`scala-yaml` / Compile / exportedModule).value,
      (`semver` / Compile / exportedModule).value
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
    .enablePlugins(JPMSPlugin)
    .settings(
      frgaalJavaCompilerSetting,
      javaModuleName := "org.enso.benchmarks.common",
      libraryDependencies ++= GraalVM.modules ++ Seq(
        "org.openjdk.jmh"  % "jmh-core"                 % jmhVersion,
        "org.openjdk.jmh"  % "jmh-generator-annprocess" % jmhVersion,
        "jakarta.xml.bind" % "jakarta.xml.bind-api"     % jaxbVersion,
        "com.sun.xml.bind" % "jaxb-impl"                % jaxbVersion
      ),
      Compile / moduleDependencies := Seq(
        "org.openjdk.jmh"      % "jmh-core"               % jmhVersion, // Automatic module
        "jakarta.xml.bind"     % "jakarta.xml.bind-api"   % jaxbVersion,
        "jakarta.activation"   % "jakarta.activation-api" % jaActivationVersion,
        "org.graalvm.polyglot" % "polyglot"               % graalMavenPackagesVersion
      ),
      Compile / internalModuleDependencies := Seq(
        (`engine-common` / Compile / exportedModule).value
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
  .enablePlugins(JPMSPlugin)
  .settings(
    frgaalJavaCompilerSetting,
    javaModuleName := "org.enso.bench.processor",
    libraryDependencies ++= Seq(
      "jakarta.xml.bind"     % "jakarta.xml.bind-api"     % jaxbVersion,
      "com.sun.xml.bind"     % "jaxb-impl"                % jaxbVersion,
      "org.openjdk.jmh"      % "jmh-core"                 % jmhVersion                % "provided",
      "org.openjdk.jmh"      % "jmh-generator-annprocess" % jmhVersion                % "provided",
      "org.netbeans.api"     % "org-openide-util-lookup"  % netbeansApiVersion        % "provided",
      "org.graalvm.polyglot" % "polyglot"                 % graalMavenPackagesVersion % "provided",
      "junit"                % "junit"                    % junitVersion              % Test,
      "com.github.sbt"       % "junit-interface"          % junitIfVersion            % Test,
      "org.graalvm.regex"    % "regex"                    % graalMavenPackagesVersion % Test,
      "org.graalvm.truffle"  % "truffle-api"              % graalMavenPackagesVersion % Test
    ),
    Compile / javacOptions := ((Compile / javacOptions).value ++
    // Only run ServiceProvider processor and ignore those defined in META-INF, thus
    // fixing incremental compilation setup
    Seq(
      "-processor",
      "org.netbeans.modules.openide.util.ServiceProviderProcessor"
    )),
    Compile / moduleDependencies := Seq(
      "org.graalvm.polyglot" % "polyglot"                % graalMavenPackagesVersion,
      "org.netbeans.api"     % "org-openide-util-lookup" % netbeansApiVersion
    ),
    Compile / internalModuleDependencies := Seq(
      (`engine-common` / Compile / exportedModule).value,
      (`runtime` / Compile / exportedModule).value,
      (`polyglot-api` / Compile / exportedModule).value,
      (`benchmarks-common` / Compile / exportedModule).value
    ),
    mainClass := Some("org.enso.benchmarks.libs.LibBenchRunner"),
    commands += WithDebugCommand.withDebug,
    (Test / fork) := true,
    (Test / parallelExecution) := false,
    (Test / javaOptions) ++=
      Seq(
        "-Dpolyglot.engine.WarnInterpreterOnly=false",
        "-Dpolyglotimpl.DisableClassPathIsolation=true"
      )
  )
  .dependsOn(`benchmarks-common`)
  .dependsOn(`polyglot-api`)
  .dependsOn(runtime)

lazy val `std-benchmarks` = (project in file("std-bits/benchmarks"))
  .enablePlugins(JPMSPlugin)
  .enablePlugins(PackageListPlugin)
  .settings(
    // Do not pass --limit-modules to frgaal. We need to ensure that the boot module layer
    // (for the annotation processor) contains all the truffle modules, including our
    // `org.enso.runtime` module.
    frgaalShouldNotLimitModules := true,
    frgaalJavaCompilerSetting,
    annotationProcSetting,
    libraryDependencies ++= GraalVM.modules ++ GraalVM.langsPkgs ++ GraalVM.toolsPkgs ++ Seq(
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
      (`bench-processor` / mainClass).value,
    Compile / javacOptions ++= Seq(
      "-Xlint:unchecked"
    ),
    // Passing these arguments with -J prefix will force frgaal to put the
    // arguments directly to java, rather than passing them via an argfile.
    // This means that this will correctly form the module boot layer and
    // we will have truffle modules on module-path
    Compile / javacOptions ++= {
      val mp    = (Compile / modulePath).value
      val mpStr = mp.map(_.getAbsolutePath).mkString(File.pathSeparator)
      Seq(
        "-J-Dorg.enso.benchmarks.processor.BenchProcessor.modulePath=" + mpStr
      )
    },
    Compile / javacOptions ++= Seq(
      "-processor",
      "org.enso.benchmarks.processor.BenchProcessor,org.openjdk.jmh.generators.BenchmarkProcessor",
      // There is no Truffle compiler available for annotation processors. Suppress the warning.
      "-J-Dpolyglot.engine.WarnInterpreterOnly=false"
    ),
    Compile / moduleDependencies := {
      (`runtime-benchmarks` / Compile / moduleDependencies).value
    },
    (Compile / internalModuleDependencies) := {
      val runtimeBenchsDeps =
        (`runtime-benchmarks` / Compile / internalModuleDependencies).value
      runtimeBenchsDeps ++ Seq(
        (`bench-processor` / Compile / exportedModule).value,
        (`benchmarks-common` / Compile / exportedModule).value
      )
    },
    Compile / addModules := Seq(
      (`runtime` / javaModuleName).value,
      (`bench-processor` / javaModuleName).value,
      (`benchmarks-common` / javaModuleName).value
    ),
    // std benchmark sources are patch into the `org.enso.runtime` module
    Compile / patchModules := {
      val runtimeModName = (`runtime` / javaModuleName).value
      val javaSrcDir     = (Compile / javaSource).value
      val classesDir     = (Compile / productDirectories).value.head
      Map(
        runtimeModName -> Seq(
          javaSrcDir,
          classesDir
        )
      )
    },
    // jmh is in unnamed modules
    Compile / addReads := {
      val runtimeModName = (`runtime` / javaModuleName).value
      Map(
        runtimeModName -> Seq(
          "ALL-UNNAMED",
          (`benchmarks-common` / javaModuleName).value,
          (`bench-processor` / javaModuleName).value
        )
      )
    },
    // export all the packages to ALL-UNNAMED
    Compile / addExports := {
      val runtimeModName = (`runtime` / javaModuleName).value
      val pkgs           = (Compile / packages).value
      val pkgsExports = pkgs.map { pkg =>
        runtimeModName + "/" + pkg -> Seq("ALL-UNNAMED")
      }.toMap

      pkgsExports ++ Map(
        "org.slf4j.nop/org.slf4j.nop" -> Seq("org.slf4j")
      )
    },
    javaOptions ++= {
      Seq(
        // To enable logging in benchmarks, add ch.qos.logback module on the modulePath
        //"-Dslf4j.provider=org.slf4j.nop.NOPServiceProvider"
        "-Dslf4j.provider=ch.qos.logback.classic.spi.LogbackServiceProvider"
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
  .dependsOn(`ydoc-server`)
  .dependsOn(`runtime-language-arrow`)
  .dependsOn(`syntax-rust-definition`)
  .dependsOn(`profiling-utils`)
  .dependsOn(`std-table` % "provided")
  .dependsOn(`std-base` % "provided")
  .dependsOn(`benchmark-java-helpers` % "provided")

lazy val editions = project
  .in(file("lib/scala/editions"))
  .enablePlugins(JPMSPlugin)
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    scalaModuleDependencySetting,
    compileOrder := CompileOrder.ScalaThenJava, // Note [JPMS Compile order]
    libraryDependencies ++= Seq(
      "io.circe"      %% "circe-core" % circeVersion     % "provided",
      "org.yaml"       % "snakeyaml"  % snakeyamlVersion % "provided",
      "org.scalatest" %% "scalatest"  % scalatestVersion % Test
    ),
    Compile / moduleDependencies ++= Seq(
      "org.yaml" % "snakeyaml" % snakeyamlVersion
    ),
    Compile / internalModuleDependencies := Seq(
      (`scala-yaml` / Compile / exportedModule).value,
      (`version-output` / Compile / exportedModule).value,
      (`semver` / Compile / exportedModule).value,
      (`scala-libs-wrapper` / Compile / exportedModule).value
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
  .dependsOn(`version-output`)
  .dependsOn(testkit % Test)

lazy val semver = project
  .in(file("lib/scala/semver"))
  .enablePlugins(JPMSPlugin)
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    scalaModuleDependencySetting,
    // Note [JPMS Compile order]
    compileOrder := CompileOrder.JavaThenScala,
    javaModuleName := "org.enso.semver",
    libraryDependencies ++= Seq(
      "io.circe"      %% "circe-core"      % circeVersion     % "provided",
      "org.yaml"       % "snakeyaml"       % snakeyamlVersion % "provided",
      "org.scalatest" %% "scalatest"       % scalatestVersion % Test,
      "junit"          % "junit"           % junitVersion     % Test,
      "com.github.sbt" % "junit-interface" % junitIfVersion   % Test
    ),
    Compile / moduleDependencies ++= Seq(
      "org.yaml" % "snakeyaml" % snakeyamlVersion
    ),
    Compile / internalModuleDependencies := Seq(
      (`scala-yaml` / Compile / exportedModule).value
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
  .enablePlugins(JPMSPlugin)
  .settings(
    frgaalJavaCompilerSetting,
    scalaModuleDependencySetting,
    mixedJavaScalaProjectSetting,
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
    ),
    javaModuleName := "org.enso.downloader",
    Compile / moduleDependencies ++= Seq(
      "commons-io"         % "commons-io"       % commonsIoVersion,
      "org.apache.commons" % "commons-compress" % commonsCompressVersion,
      "org.slf4j"          % "slf4j-api"        % slf4jVersion
    ),
    Compile / internalModuleDependencies := Seq(
      (`cli` / Compile / exportedModule).value,
      (`scala-libs-wrapper` / Compile / exportedModule).value
    )
  )
  .dependsOn(cli)
  .dependsOn(`http-test-helper` % "test->test")
  .dependsOn(testkit % Test)

lazy val `edition-updater` = project
  .in(file("lib/scala/edition-updater"))
  .enablePlugins(JPMSPlugin)
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    scalaModuleDependencySetting,
    compileOrder := CompileOrder.ScalaThenJava, // Note [JPMS Compile order]
    Test / test := (Test / test).tag(simpleLibraryServerTag).value,
    libraryDependencies ++= Seq(
      "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion,
      "org.scalatest"              %% "scalatest"     % scalatestVersion % Test
    ),
    Compile / internalModuleDependencies := Seq(
      (`distribution-manager` / Compile / exportedModule).value,
      (`editions` / Compile / exportedModule).value
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
  .enablePlugins(JPMSPlugin)
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    scalaModuleDependencySetting,
    compileOrder := CompileOrder.ScalaThenJava, // Note [JPMS Compile order]
    libraryDependencies ++= Seq(
      "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion,
      "org.scalatest"              %% "scalatest"     % scalatestVersion % Test
    ),
    javaModuleName := "org.enso.librarymanager",
    Compile / moduleDependencies ++= Seq(
      "org.yaml"  % "snakeyaml" % snakeyamlVersion,
      "org.slf4j" % "slf4j-api" % slf4jVersion
    ),
    Compile / internalModuleDependencies := Seq(
      (`distribution-manager` / Compile / exportedModule).value,
      (`downloader` / Compile / exportedModule).value,
      (`cli` / Compile / exportedModule).value,
      (`editions` / Compile / exportedModule).value,
      (`pkg` / Compile / exportedModule).value,
      (`semver` / Compile / exportedModule).value,
      (`logging-utils` / Compile / exportedModule).value,
      (`scala-libs-wrapper` / Compile / exportedModule).value,
      (`scala-yaml` / Compile / exportedModule).value
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
  .enablePlugins(JPMSPlugin)
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    scalaModuleDependencySetting,
    compileOrder := CompileOrder.ScalaThenJava,
    Test / fork := true,
    commands += WithDebugCommand.withDebug,
    Test / javaOptions ++= testLogProviderOptions,
    Test / test := (Test / test).tag(simpleLibraryServerTag).value,
    libraryDependencies ++= Seq(
      "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion,
      "org.scalatest"              %% "scalatest"     % scalatestVersion % Test
    ),
    Compile / internalModuleDependencies := Seq(
      (`library-manager` / Compile / exportedModule).value,
      (`cli` / Compile / exportedModule).value,
      (`distribution-manager` / Compile / exportedModule).value,
      (`library-manager` / Compile / exportedModule).value,
      (`process-utils` / Compile / exportedModule).value,
      (`pkg` / Compile / exportedModule).value,
      (`semver` / Compile / exportedModule).value,
      (`downloader` / Compile / exportedModule).value,
      (`editions` / Compile / exportedModule).value,
      (`version-output` / Compile / exportedModule).value,
      (`testkit` / Compile / exportedModule).value
    )
  )
  .dependsOn(`library-manager`)
  .dependsOn(`process-utils`)
  .dependsOn(`logging-utils` % "test->test")
  .dependsOn(testkit)
  .dependsOn(`logging-service-logback` % "test->test")

lazy val `connected-lock-manager` = project
  .in(file("lib/scala/connected-lock-manager"))
  .enablePlugins(JPMSPlugin)
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    scalaModuleDependencySetting,
    compileOrder := CompileOrder.ScalaThenJava,
    libraryDependencies ++= Seq(
      "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion,
      "org.scalatest"              %% "scalatest"     % scalatestVersion % Test
    ),
    Compile / internalModuleDependencies := Seq(
      (`distribution-manager` / Compile / exportedModule).value,
      (`polyglot-api` / Compile / exportedModule).value,
      (`scala-libs-wrapper` / Compile / exportedModule).value
    )
  )
  .dependsOn(`distribution-manager`)
  .dependsOn(`connected-lock-manager-server` % "test->test")
  .dependsOn(`polyglot-api`)

/** Unlike `connected-lock-manager` project, has a dependency on akka.
  */
lazy val `connected-lock-manager-server` = project
  .in(file("lib/scala/connected-lock-manager-server"))
  .enablePlugins(JPMSPlugin)
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    scalaModuleDependencySetting,
    mixedJavaScalaProjectSetting,
    libraryDependencies ++= Seq(
      "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion,
      akkaActor,
      akkaTestkit      % Test,
      "org.scalatest" %% "scalatest" % scalatestVersion % Test
    ),
    Compile / internalModuleDependencies := Seq(
      (`scala-libs-wrapper` / Compile / exportedModule).value,
      (`akka-wrapper` / Compile / exportedModule).value,
      (`distribution-manager` / Compile / exportedModule).value,
      (`polyglot-api` / Compile / exportedModule).value
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
  .enablePlugins(JPMSPlugin)
  .settings(
    frgaalJavaCompilerSetting,
    scalaModuleDependencySetting,
    compileOrder := CompileOrder.ScalaThenJava
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
  .enablePlugins(JPMSPlugin)
  .settings(
    frgaalJavaCompilerSetting,
    autoScalaLibrary := false,
    Compile / packageBin / artifactPath :=
      `base-polyglot-root` / "common-polyglot-core-utils.jar",
    libraryDependencies ++= Seq(
      "com.ibm.icu"          % "icu4j"    % icuVersion,
      "org.graalvm.polyglot" % "polyglot" % graalMavenPackagesVersion % "provided"
    ),
    Compile / moduleDependencies := Seq(
      "com.ibm.icu" % "icu4j" % icuVersion
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
      "org.openpnp"          % "opencv"                  % opencvVersion
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
      "com.google.api-client" % "google-api-client"          % googleApiClientVersion exclude ("com.google.code.findbugs", "jsr305"),
      "com.google.apis"       % "google-api-services-sheets" % googleApiServicesSheetsVersion exclude ("com.google.code.findbugs", "jsr305"),
      "com.google.analytics"  % "google-analytics-data"      % googleAnalyticsDataVersion exclude ("com.google.code.findbugs", "jsr305")
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
      "org.postgresql"       % "postgresql"              % postgresVersion
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
            import scala.concurrent.ExecutionContext.Implicits.global
            implicit val filesNotEmptySuccess: retry.Success[Set[File]] =
              retry.Success(!_.isEmpty)
            import scala.concurrent.duration._
            val future = retry.Backoff(4, 1.second).apply { () =>
              scala.concurrent.Future {
                try {
                  IO.unzipURL(
                    unmanagedExternalZip.value,
                    tmp,
                    f =>
                      f.endsWith(".jar") && !f.contains("gradle") && !f
                        .contains(
                          "javadoc"
                        ) && !f.contains("jna")
                  )
                } catch {
                  case _: java.net.SocketException |
                      _: java.net.ConnectException =>
                    Set.empty[File]
                }
              }
            }
            future.onComplete { result =>
              if (result.isFailure || result.get.isEmpty) {
                logger.log(
                  Level.Error,
                  "Failed to fetch any external artifacts for tableau"
                )
              }
            }
            val files = scala.concurrent.Await.result(future, 60.seconds)
            if (files.isEmpty) {
              logger.log(
                Level.Error,
                "Failed to fetch any external artifacts for tableau"
              )
              throw new IllegalStateException(
                "Failed to fetch any external artifacts"
              )
            }
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
  val log          = streams.value.log
  val cacheFactory = streams.value.cacheStoreFactory
  val libraries = Editions.standardLibraries.map(libName =>
    BundledLibrary(libName, stdLibVersion)
  )
  val runnerCp   = (`engine-runner` / Runtime / fullClasspath).value
  val runtimeCp  = (`runtime` / Runtime / fullClasspath).value
  val fullCp     = (runnerCp ++ runtimeCp).distinct
  val modulePath = componentModulesPaths.value
  val javaOpts = Seq(
    "--module-path",
    modulePath.map(_.getAbsolutePath).mkString(File.pathSeparator),
    "-m",
    "org.enso.runner/org.enso.runner.Main"
  )
  LibraryManifestGenerator.generateManifests(
    libraries,
    file("distribution"),
    log,
    javaOpts,
    cacheFactory
  )
}
