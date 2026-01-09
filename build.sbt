import org.enso.build.BenchTasks.*
import org.enso.build.WithDebugCommand
import org.apache.commons.io.FileUtils
import sbt.Keys.{libraryDependencies, scalacOptions}
import sbt.addCompilerPlugin
import sbt.complete.DefaultParsers.*
import sbt.complete.Parser
import sbt.nio.file.FileTreeView
import sbt.internal.util.ManagedLogger
import src.main.scala.licenses.{
  DistributionDescription,
  SBTDistributionComponent
}

import scala.sys.process.*
import Dependencies.*
import JarExtractor.{
  CopyToOutputJar,
  LinuxAMD64,
  MacOSArm64,
  PolyglotLib,
  WindowsAMD64
}

import java.nio.file.{Files, StandardCopyOption}

// This import is unnecessary, but bit adds a proper code completion features
// to IntelliJ.
import JPMSPlugin.autoImport._
import PackageListPlugin.autoImport._
import BazelSupport.autoImport._
import JarExtractPlugin.autoImport._

import java.io.File

// ============================================================================
// === Global Configuration ===================================================
// ============================================================================

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

ThisBuild / organization := "org.enso"
ThisBuild / scalaVersion := scalacVersion
ThisBuild / publish / skip := true
ThisBuild / assembly / logLevel := Level.Warn

/* Tag limiting the concurrent access to tools/simple-library-server in tests.
 */
val simpleLibraryServerTag = Tags.Tag("simple-library-server")
Global / concurrentRestrictions += Tags.limit(simpleLibraryServerTag, 1)

/** Tag limiting the concurrent spawning of `native-image` subprocess.
  */
val nativeImageBuildTag = NativeImage.nativeImageBuildTag
Global / concurrentRestrictions += Tags.limit(nativeImageBuildTag, 1)

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
  makeStdLibDistribution("Base", Distribution.sbtProjects(`std-base`)),
  makeStdLibDistribution(
    "Generic_JDBC",
    Distribution.sbtProjects(`std-generic-jdbc`)
  ),
  makeStdLibDistribution(
    "Google",
    Distribution.sbtProjects(`std-google`)
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
  makeStdLibDistribution(
    "Tableau",
    Distribution.sbtProjects(`std-tableau`, `jna-wrapper`)
  ),
  makeStdLibDistribution(
    "Saas",
    Distribution.sbtProjects(`std-saas`)
  ),
  makeStdLibDistribution(
    "DuckDB",
    Distribution.sbtProjects(`std-duckdb`)
  )
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

lazy val distributionArtifactRoot = SettingKey[File](
  "root directory where distribution artifacts will be built."
)
distributionArtifactRoot := {
  if ((Bazel / wasStartedFromBazel).value) {
    (Bazel / outputDir).value.get
  } else {
    file("built-distribution")
  }
}

lazy val packageBuilder = SettingKey[DistributionPackage.Builder](
  "create package builder with correct output dir path"
)
packageBuilder := {
  val artifactRoot = distributionArtifactRoot.value
  new DistributionPackage.Builder(
    ensoVersion      = ensoVersion,
    graalVersion     = graalMavenPackagesVersion,
    graalJavaVersion = graalVersion,
    artifactRoot     = artifactRoot
  )
}

lazy val checkIRCacheSizes = taskKey[Unit](
  "Checks that the IR caches of all standard libraries are within the size limit."
)
checkIRCacheSizes := Def
  .task {
    val stdLibRoot =
      engineDistributionRoot.value / "lib" / "Standard"
    IRCaches.checkCacheSizes(
      stdLibRoot  = stdLibRoot,
      ensoVersion = ensoVersion,
      log         = streams.value.log
    )
  }
  .dependsOn(buildEngineDistribution)
  .value

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

ThisBuild / javaOptions ++= Seq(
  // Truffle calls terminally deprecated methods from sun.misc.Unsafe in JDK24.
  // This removes the warnings at runtime.
  // TODO: Remove this until JDK 26
  "--sun-misc-unsafe-memory-access=allow"
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

// Native Image Generation
lazy val rebuildNativeImage = taskKey[Unit]("Force to rebuild native image")
lazy val buildNativeImage =
  taskKey[Unit]("Ensure that the Native Image is built.")
lazy val checkNativeImageSize =
  taskKey[Unit]("Ensures the generated Native Image has reasonable size")

// ============================================================================
// === Global Project =========================================================
// ============================================================================

lazy val enso = (project in file("."))
  .settings(version := "0.1")
  .aggregate(
    `akka-wrapper`,
    `benchmark-java-helpers`,
    `benchmarks-common`,
    `bench-processor`,
    cli,
    `common-polyglot-core-utils`,
    `connected-lock-manager`,
    `connected-lock-manager-server`,
    `distribution-manager`,
    downloader,
    editions,
    `edition-updater`,
    `engine-common`,
    `engine-runner`,
    `engine-runner-common`,
    `generic-jdbc-connection-spec-dependencies`,
    `enso-test-java-helpers`,
    `snowflake-test-java-helpers`,
    `exploratory-benchmark-java-helpers`,
    `fansi-wrapper`,
    filewatcher,
    `http-test-helper`,
    `interpreter-dsl`,
    `interpreter-dsl-test`,
    `jna-wrapper`,
    `jna-wrapper-extracted`,
    `json-rpc-server`,
    `jvm-channel`,
    `jvm-interop`,
    `language-server`,
    `language-server-deps-wrapper`,
    launcher,
    `library-manager`,
    `locking-test-helper`,
    `logging-config`,
    `logging-service`,
    `logging-service-logback`,
    `logging-service-common`,
    `logging-service-opensearch`,
    `logging-service-telemetry`,
    `logging-truffle-connector`,
    `logging-utils`,
    `logging-utils-akka`,
    `netty-epoll-native-wrapper`,
    `netty-tc-native-wrapper`,
    `netty-resolver-dns-native-macos-wrapper`,
    `opencv-wrapper`,
    `os-environment`,
    `os-environment-lib`,
    `persistance`,
    `persistance-dsl`,
    pkg,
    `poi-wrapper`,
    `polyglot-api`,
    `polyglot-api-macros`,
    `process-utils`,
    `profiling-utils`,
    `python-extract`,
    `python-resource-provider`,
    `refactoring-utils`,
    runtime,
    `runtime-and-langs`,
    `runtime-benchmarks`,
    `runtime-compiler`,
    `runtime-compiler-dump`,
    `runtime-compiler-dump-igv`,
    `runtime-parser`,
    `runtime-parser-dsl`,
    `runtime-parser-processor`,
    `runtime-parser-processor-tests`,
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
    `runtime-test-instruments`,
    `runtime-utils`,
    `scala-libs-wrapper`,
    `scala-yaml`,
    searcher,
    semver,
    `std-aws`,
    `std-base`,
    `std-benchmarks`,
    `std-database`,
    `std-generic-jdbc`,
    `std-google`,
    `std-image`,
    `std-microsoft`,
    `std-snowflake`,
    `std-table`,
    `std-tests`,
    `std-tableau`,
    `std-saas`,
    `std-duckdb`,
    `sqlite-wrapper`,
    `syntax-rust-definition`,
    `tableau-wrapper`,
    `task-progress-notifications`,
    testkit,
    `test-utils`,
    `text-buffer`,
    `version-output`,
    `ydoc-polyfill`,
    `ydoc-server`,
    `ydoc-server-registration`,
    `zio-wrapper`
  )
  .settings(Global / concurrentRestrictions += Tags.exclusive(Exclusive))
  .settings(
    commands ++= {
      Seq(
        packageBuilder.value.makePackages,
        packageBuilder.value.makeBundles
      )
    }
  )
  .settings(
    clean := Def.task {
      val _ = clean.value
      val filesToDelete = Seq(
        engineDistributionRoot.value,
        launcherDistributionRoot.value,
        packageBuilder.value.artifactRoot
      )
      IO.delete(filesToDelete)
    }.value
  )

// ============================================================================
// === Utility methods =====================================================
// ============================================================================

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
    scalaReflect ++
    helidon ++
    scalaLibrary ++
    logbackPkg ++
    jline ++
    slf4jApi ++
    Seq(
      "org.netbeans.api"       % "org-netbeans-modules-sampler" % netbeansApiVersion,
      "com.google.flatbuffers" % "flatbuffers-java"             % flatbuffersVersion,
      "com.google.protobuf"    % "protobuf-java"                % googleProtobufVersion,
      "commons-cli"            % "commons-cli"                  % commonsCliVersion,
      "commons-io"             % "commons-io"                   % commonsIoVersion,
      "org.yaml"               % "snakeyaml"                    % snakeyamlVersion,
      "org.eclipse.jgit"       % "org.eclipse.jgit"             % jgitVersion,
      "com.typesafe"           % "config"                       % typesafeConfigVersion,
      "org.reactivestreams"    % "reactive-streams"             % reactiveStreamsVersion,
      "org.apache.commons"     % "commons-lang3"                % commonsLangVersion,
      "org.apache.commons"     % "commons-compress"             % commonsCompressVersion,
      "org.apache.tika"        % "tika-core"                    % tikaVersion,
      "org.yaml"               % "snakeyaml"                    % snakeyamlVersion,
      "com.ibm.icu"            % "icu4j"                        % icuVersion
    )
  val modsToExclude = jlineNative ++ Seq(
    "org.graalvm.python" % "python-resources" % Dependencies.graalMavenPackagesVersion
  )
  val reducedThirdPartyModIds = thirdPartyModIds.filterNot { modId =>
    modsToExclude.exists { excludedMod =>
      modId.organization == excludedMod.organization &&
      modId.name == excludedMod.name &&
      modId.revision == excludedMod.revision
    }
  }

  val thirdPartyMods = JPMSUtils.filterModulesFromClasspath(
    fullCp,
    reducedThirdPartyModIds,
    log,
    projName = moduleName.value,
    scalaBinaryVersion.value,
    shouldContainAll = true
  )
  val thirdPartyModFiles = thirdPartyMods.map(_.data)
  val ourMods = Seq(
    (`akka-wrapper` / Compile / exportedModuleBin).value,
    (`cli` / Compile / exportedModuleBin).value,
    (`common-polyglot-core-utils` / Compile / exportedModuleBin).value,
    (`connected-lock-manager` / Compile / exportedModuleBin).value,
    (`connected-lock-manager-server` / Compile / exportedModuleBin).value,
    (`distribution-manager` / Compile / exportedModuleBin).value,
    (`downloader` / Compile / exportedModuleBin).value,
    (`editions` / Compile / exportedModuleBin).value,
    (`edition-updater` / Compile / exportedModuleBin).value,
    (`engine-common` / Compile / exportedModuleBin).value,
    (`engine-runner` / Compile / exportedModuleBin).value,
    (`engine-runner-common` / Compile / exportedModuleBin).value,
    (`fansi-wrapper` / Compile / exportedModuleBin).value,
    (`filewatcher` / Compile / exportedModuleBin).value,
    (`json-rpc-server` / Compile / exportedModuleBin).value,
    (`jvm-channel` / Compile / exportedModuleBin).value,
    (`jvm-interop` / Compile / exportedModuleBin).value,
    (`language-server` / Compile / exportedModuleBin).value,
    (`language-server-deps-wrapper` / Compile / exportedModuleBin).value,
    (`library-manager` / Compile / exportedModuleBin).value,
    (`library-manager` / Compile / exportedModuleBin).value,
    (`logging-config` / Compile / exportedModuleBin).value,
    (`logging-service` / Compile / exportedModuleBin).value,
    (`logging-service-common` / Compile / exportedModuleBin).value,
    (`logging-service-logback` / Compile / exportedModuleBin).value,
    (`logging-service-opensearch` / Compile / exportedModuleBin).value,
    (`logging-service-telemetry` / Compile / exportedModuleBin).value,
    (`logging-utils` / Compile / exportedModuleBin).value,
    (`logging-utils-akka` / Compile / exportedModuleBin).value,
    (`os-environment` / Compile / exportedModuleBin).value,
    (`persistance` / Compile / exportedModuleBin).value,
    (`pkg` / Compile / exportedModuleBin).value,
    (`process-utils` / Compile / exportedModuleBin).value,
    (`profiling-utils` / Compile / exportedModuleBin).value,
    (`polyglot-api` / Compile / exportedModuleBin).value,
    (`polyglot-api-macros` / Compile / exportedModuleBin).value,
    (`python-resource-provider` / Compile / exportedModuleBin).value,
    (`refactoring-utils` / Compile / exportedModuleBin).value,
    (`runtime` / Compile / exportedModuleBin).value,
    (`runtime-compiler` / Compile / exportedModuleBin).value,
    (`runtime-compiler-dump` / Compile / exportedModuleBin).value,
    (`runtime-compiler-dump-igv` / Compile / exportedModuleBin).value,
    (`runtime-instrument-common` / Compile / exportedModuleBin).value,
    (`runtime-instrument-id-execution` / Compile / exportedModuleBin).value,
    (`runtime-instrument-repl-debugger` / Compile / exportedModuleBin).value,
    (`runtime-instrument-runtime-server` / Compile / exportedModuleBin).value,
    (`runtime-language-arrow` / Compile / exportedModuleBin).value,
    (`runtime-language-epb` / Compile / exportedModuleBin).value,
    (`runtime-parser` / Compile / exportedModuleBin).value,
    (`runtime-suggestions` / Compile / exportedModuleBin).value,
    (`runtime-utils` / Compile / exportedModuleBin).value,
    (`runtime-version-manager` / Compile / exportedModuleBin).value,
    (`scala-libs-wrapper` / Compile / exportedModuleBin).value,
    (`scala-yaml` / Compile / exportedModuleBin).value,
    (`searcher` / Compile / exportedModuleBin).value,
    (`semver` / Compile / exportedModuleBin).value,
    (`syntax-rust-definition` / Compile / exportedModuleBin).value,
    (`task-progress-notifications` / Compile / exportedModuleBin).value,
    (`text-buffer` / Compile / exportedModuleBin).value,
    (`version-output` / Compile / exportedModuleBin).value,
    (`ydoc-polyfill` / Compile / exportedModuleBin).value,
    (`ydoc-server` / Compile / exportedModuleBin).value,
    (`ydoc-server-registration` / Compile / exportedModuleBin).value,
    (`zio-wrapper` / Compile / exportedModuleBin).value
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

/** Mockito agent needs to be explicitly set as `-javaagent` to the JVM.
  * Note that starting agent programatically was deprecated in JDK 21 and is scheduled to be removed.
  * See https://javadoc.io/doc/org.mockito/mockito-core/latest/org.mockito/org/mockito/Mockito.html#0.3
  */
lazy val mockitoAgentSettings: SettingsDefinition = Seq(
  libraryDependencies ++= Seq(
    "org.mockito" % "mockito-core" % mockitoJavaVersion % Test
  ),
  Test / javaOptions += {
    val logger = streams.value.log
    val mockitoJar = JPMSUtils.filterModulesFromUpdate(
      update.value,
      Seq(
        "org.mockito" % "mockito-core" % mockitoJavaVersion
      ),
      logger,
      moduleName.value,
      scalaBinaryVersion.value,
      shouldContainAll = true
    )
    if (mockitoJar.length != 1) {
      logger.error(
        s"Expected exactly one mockito-core jar on the classpath, found: ${mockitoJar.map(_.name).mkString(", ")}"
      )
    }
    val mockitoJarPath = mockitoJar.head.getAbsolutePath
    s"-javaagent:$mockitoJarPath"
  }
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
    annotationProcSetting,
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
`syntax-rust-definition` / generateRustParserLib := Def.taskIf {
  if ((`syntax-rust-definition` / Bazel / wasStartedFromBazel).value) {
    val libName = System.mapLibraryName("enso_parser")
    val libDest =
      (`syntax-rust-definition` / rustParserTargetDirectory).value / libName
    val libFrombazel =
      (`syntax-rust-definition` / Bazel / rustParserLib).value
    IO.copyFile(libFrombazel, libDest)
    Seq(
      libDest
    )
  } else {
    val log        = state.value.log
    val profile    = if (BuildInfo.isReleaseMode) "release" else "dev"
    val profileDir = if (BuildInfo.isReleaseMode) "release" else "debug"
    val libName    = System.mapLibraryName("enso_parser")
    // The library will be copied into this location. It is required in various
    // other places.
    val copyLibDest =
      (`syntax-rust-definition` / rustParserTargetDirectory).value / libName
    val libGlob =
      (`syntax-rust-definition` / rustParserTargetDirectory).value.toGlob / libName

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
      // Destination of the native library as built by Cargo
      val libDest = target match {
        case Some(someTarget) =>
          (`syntax-rust-definition` / rustParserTargetDirectory).value / someTarget / profileDir / libName
        case None =>
          (`syntax-rust-definition` / rustParserTargetDirectory).value / profileDir / libName
      }
      target.foreach { t =>
        Cargo.rustUp(t, log)
      }
      val arguments = Seq(
        "build",
        "-p",
        "enso-parser-jni",
        "--profile",
        profile
      ) ++ target.map(t => Seq("--target", t)).getOrElse(Seq()) ++
        Seq(
          "--target-dir",
          (`syntax-rust-definition` / rustParserTargetDirectory).value.toString
        )
      val envVars = target
        .map(_ => Seq(("RUSTFLAGS", "-C target-feature=-crt-static")))
        .getOrElse(Seq())
      Cargo.run(arguments, log, envVars)
      if (!libDest.exists()) {
        log.error(
          s"Expected Rust parser library at ${libDest.toPath} but it does not exist after build."
        )
      }
      Files.copy(
        libDest.toPath,
        copyLibDest.toPath,
        StandardCopyOption.REPLACE_EXISTING
      )
      if (!Files.exists(copyLibDest.toPath)) {
        log.error(
          s"Failed to copy Rust parser library to ${copyLibDest.toPath}."
        )
      }
    }
    FileTreeView.default.list(Seq(libGlob)).map(_._1.toFile)
  }
}.value

`syntax-rust-definition` / generateRustParserLib / fileInputs := {
  if ((`syntax-rust-definition` / Bazel / wasStartedFromBazel).value) {
    Seq.empty
  } else {
    Seq(
      (`syntax-rust-definition` / baseDirectory).value.toGlob / "jni" / "src" / ** / "*.rs",
      (`syntax-rust-definition` / baseDirectory).value.toGlob / "src" / ** / "*.rs"
    )
  }
}

val generateParserJavaSources = TaskKey[Seq[File]](
  "generateParserJavaSources",
  "Generates Java sources for Rust parser"
)
`syntax-rust-definition` / generateParserJavaSources := Def.taskIf {
  import scala.jdk.CollectionConverters._
  if ((`syntax-rust-definition` / Bazel / wasStartedFromBazel).value) {
    // Copy the generated sources from Bazel directory to our directory
    val srcsFromBazel =
      (`syntax-rust-definition` / Bazel / rustParserJavaSources).value
    val base   = (`syntax-rust-definition` / Compile / sourceManaged).value
    val outDir = base / "org" / "enso" / "syntax2"
    if (!outDir.exists()) {
      outDir.mkdirs()
      srcsFromBazel.foreach { src =>
        val fname = src.getName
        val dest  = outDir / fname
        IO.copyFile(src, dest)
      }
    }
    FileUtils.listFiles(outDir, Array("scala", "java"), true).asScala.toSeq
  } else {
    val base   = (`syntax-rust-definition` / Compile / sourceManaged).value
    val outDir = base / "org" / "enso" / "syntax2"
    generateRustParser(
      outDir,
      (`syntax-rust-definition` / generateParserJavaSources).inputFileChanges,
      state.value.log
    )
  }
}.value

`syntax-rust-definition` / generateParserJavaSources / fileInputs := {
  if ((`syntax-rust-definition` / Bazel / wasStartedFromBazel).value) {
    Seq.empty
  } else {
    Seq(
      (`syntax-rust-definition` / baseDirectory).value.toGlob / "generate-java" / "src" / ** / "*.rs",
      (`syntax-rust-definition` / baseDirectory).value.toGlob / "src" / ** / "*.rs"
    )
  }
}

/** Generates Java sources via `enso-parser-generate-java` binary.
  * That binary must already exist - created via Rust compilation.
  * @param outDir Base directory where the sources will be put.
  *             No package hierarchy will be created.
  */
def generateRustParser(
  outDir: File,
  changes: sbt.nio.FileChanges,
  log: ManagedLogger
): Seq[File] = {
  import scala.jdk.CollectionConverters._

  if (!outDir.exists()) {
    outDir.mkdirs()
  }
  if (changes.hasChanges) {
    val args = Seq(
      "run",
      "-p",
      "enso-parser-generate-java",
      "--bin",
      "enso-parser-generate-java",
      outDir.toString
    )
    Cargo.run(args, log)
  }
  FileUtils.listFiles(outDir, Array("scala", "java"), true).asScala.toSeq
}

lazy val `syntax-rust-definition` = project
  .in(file("lib/rust/parser"))
  .enablePlugins(BazelSupport && JPMSPlugin)
  .configs(Test)
  .settings(
    javadocSettings,
    publishLocalSetting,
    Compile / exportJars := true,
    autoScalaLibrary := false,
    crossPaths := false,
    libraryDependencies ++= slf4jApi,
    Compile / moduleDependencies ++= slf4jApi,
    javaModuleName := "org.enso.syntax",
    Compile / sourceGenerators += generateParserJavaSources,
    Compile / resourceGenerators += generateRustParserLib,
    Compile / javaSource := baseDirectory.value / "generate-java" / "java",
    Compile / compile / javacOptions ++= Seq("-source", "11", "-target", "11"),
    // Make sure the native library is not packaged in the exported `jar`.
    assembly / assemblyMergeStrategy := {
      case PathList(file)
          if file.endsWith(".so") || file.endsWith(".dll") || file
            .endsWith(".dylib") =>
        MergeStrategy.discard
      case _ =>
        MergeStrategy.first
    },
    assembly / assemblyExcludedJars := {
      JPMSUtils.filterModulesFromClasspath(
        (Compile / fullClasspath).value,
        slf4jApi,
        streams.value.log,
        javaModuleName.value,
        scalaBinaryVersion.value,
        shouldContainAll = true
      )
    },
    Compile / exportedModule := assembly.value,
    Compile / exportedModuleBin := assembly.value
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
    mixedJavaScalaProjectSetting,
    annotationProcSetting,
    version := "0.1",
    Compile / run / mainClass := Some("org.enso.pkg.Main"),
    libraryDependencies ++= Seq(
      "org.graalvm.sdk"    % "nativeimage"      % graalMavenPackagesVersion % "provided",
      "io.circe"          %% "circe-core"       % circeVersion              % "provided",
      "org.yaml"           % "snakeyaml"        % snakeyamlVersion          % "provided",
      "org.scalatest"     %% "scalatest"        % scalatestVersion          % Test,
      "org.apache.commons" % "commons-compress" % commonsCompressVersion
    ),
    Compile / moduleDependencies ++= Seq(
      "org.graalvm.sdk"    % "word"             % graalMavenPackagesVersion,
      "org.graalvm.sdk"    % "nativeimage"      % graalMavenPackagesVersion,
      "org.apache.commons" % "commons-compress" % commonsCompressVersion,
      "org.yaml"           % "snakeyaml"        % snakeyamlVersion
    ),
    Compile / internalModuleDependencies := Seq(
      (`editions` / Compile / exportedModule).value,
      (`scala-libs-wrapper` / Compile / exportedModule).value,
      (`scala-yaml` / Compile / exportedModule).value,
      (`semver` / Compile / exportedModule).value,
      (`version-output` / Compile / exportedModule).value
    )
  )
  .dependsOn(editions)

lazy val extractPythonResources = taskKey[Seq[File]](
  "Extract python resources from the python-resource.jar"
)
// Project that extracts python-resources during build time.
lazy val `python-extract` = project
  .in(file("lib/java/python-extract"))
  .settings(
    frgaalJavaCompilerSetting,
    libraryDependencies ++= Seq(
      "org.graalvm.python" % "python-language"  % graalMavenPackagesVersion,
      "org.graalvm.python" % "python-resources" % graalMavenPackagesVersion
    ),
    Compile / run / mainClass := Some("org.enso.pyextract.PythonExtract"),
    Compile / run / javaOptions ++= Seq("--enable-native-access=ALL-UNNAMED"),
    Compile / run / fork := true,
    extractPythonResources := Def.taskIf {
      if ((Bazel / wasStartedFromBazel).value) {
        val resDir = (Bazel / extractedPythonResourceDir).value
        val glob   = resDir.toGlob / ** / *
        FileTreeView.default.list(Seq(glob)).map(_._1.toFile)
      } else {
        val outDir          = target.value / "python-resources"
        val pyResourcesGlob = target.value.toGlob / "python-resources" / ** / *
        val logger          = streams.value.log
        val outs            = FileTreeView.default.list(Seq(pyResourcesGlob)).map(_._1)
        val main            = (Compile / run / mainClass).value
        val classPath       = (Compile / fullClasspath).value
        val args = Seq(
          outDir.getPath
        )
        val javaRunner = (Compile / run / runner).value
        if (outs.isEmpty) {
          javaRunner.run(
            main.get,
            classPath.files,
            args,
            logger
          )
        }
        FileTreeView.default.list(Seq(pyResourcesGlob)).map(_._1.toFile)
      }
    }.value,
    clean := {
      val _      = clean.value
      val outDir = target.value / "python-resources"
      IO.delete(outDir)
    }
  )

lazy val `python-resource-provider` = project
  .in(file("engine/python-resource-provider"))
  .enablePlugins(JPMSPlugin)
  .settings(
    frgaalJavaCompilerSetting,
    libraryDependencies ++= Seq(
      "org.graalvm.truffle" % "truffle-api" % graalMavenPackagesVersion
    ),
    Compile / moduleDependencies ++= Seq(
      "org.graalvm.truffle" % "truffle-api" % graalMavenPackagesVersion,
      "org.graalvm.sdk"     % "word"        % graalMavenPackagesVersion
    )
  )

lazy val `profiling-utils` = project
  .in(file("lib/scala/profiling-utils"))
  .enablePlugins(JPMSPlugin)
  .configs(Test)
  .settings(
    customFrgaalJavaCompilerSettings(targetJdk = "21"),
    compileOrder := CompileOrder.JavaThenScala,
    javaModuleName := "org.enso.profiling",
    Compile / exportJars := true,
    version := "0.1",
    libraryDependencies ++= slf4jApi ++ Seq(
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
      "junit"          % "junit"           % junitVersion   % Test,
      "com.github.sbt" % "junit-interface" % junitIfVersion % Test
    ),
    Compile / moduleDependencies ++= slf4jApi ++
    Seq(
      "org.netbeans.api" % "org-netbeans-modules-sampler" % netbeansApiVersion
    )
  )

lazy val `logging-utils` = project
  .in(file("lib/scala/logging-utils"))
  .enablePlugins(JPMSPlugin)
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    scalaModuleDependencySetting,
    annotationProcSetting,
    compileOrder := CompileOrder.ScalaThenJava, // Note [JPMS Compile order]
    version := "0.1",
    libraryDependencies ++= slf4jApi ++ Seq(
      "org.scalatest" %% "scalatest" % scalatestVersion % Test
    ) ++ logbackTest,
    Compile / moduleDependencies ++= slf4jApi
  )

lazy val `logging-service` = project
  .in(file("lib/scala/logging-service"))
  .enablePlugins(JPMSPlugin)
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    scalaModuleDependencySetting,
    annotationProcSetting,
    version := "0.1",
    libraryDependencies ++= slf4jApi ++ Seq(
      "com.typesafe"   % "config"    % typesafeConfigVersion,
      "org.scalatest" %% "scalatest" % scalatestVersion % Test
    ),
    Compile / moduleDependencies ++= slf4jApi,
    Compile / internalModuleDependencies := Seq(
      (`logging-config` / Compile / exportedModule).value,
      (`logging-utils` / Compile / exportedModule).value
    )
  )
  .dependsOn(`logging-config`)
  .dependsOn(`logging-utils`)

lazy val `logging-config` = project
  .in(file("lib/scala/logging-config"))
  .enablePlugins(JPMSPlugin)
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    annotationProcSetting,
    version := "0.1",
    libraryDependencies ++= Seq(
      "com.typesafe"         % "config"    % typesafeConfigVersion,
      "org.slf4j"            % "slf4j-api" % slf4jVersion,
      "org.graalvm.polyglot" % "polyglot"  % graalMavenPackagesVersion % "provided"
    ),
    Compile / moduleDependencies ++= Seq(
      "com.typesafe"         % "config"    % typesafeConfigVersion,
      "org.graalvm.polyglot" % "polyglot"  % graalMavenPackagesVersion,
      "org.slf4j"            % "slf4j-api" % slf4jVersion
    ),
    Compile / internalModuleDependencies ++= Seq(
      (`engine-common` / Compile / exportedModule).value,
      (`logging-utils` / Compile / exportedModule).value
    )
  )
  .dependsOn(`engine-common` % "provided")

lazy val `logging-service-logback` = project
  .in(file("lib/scala/logging-service-logback"))
  .enablePlugins(JPMSPlugin)
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    annotationProcSetting,
    version := "0.1",
    libraryDependencies ++= slf4jApi ++ Seq(
      "org.scalatest" %% "scalatest" % scalatestVersion % Test
    ) ++ logbackPkg,
    Compile / moduleDependencies ++= logbackPkg ++ slf4jApi,
    Compile / javaModuleName := "org.enso.logging.service.logback",
    Compile / shouldCompileModuleInfoManually := true,
    Compile / internalModuleDependencies := Seq(
      (`logging-config` / Compile / exportedModule).value,
      (`logging-service` / Compile / exportedModule).value,
      (`logging-utils` / Compile / exportedModule).value
    ),
    Test / shouldCompileModuleInfoManually := true,
    Test / javaModuleName := "org.enso.logging.service.logback.test.provider",
    Test / moduleDependencies ++= scalaLibrary,
    Test / internalModuleDependencies := Seq(
      (Compile / exportedModule).value
    )
  )
  .dependsOn(`logging-config`)
  .dependsOn(`logging-service`)

lazy val `logging-service-telemetry` = project
  .in(file("lib/java/logging-service-telemetry"))
  .enablePlugins(JPMSPlugin)
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    javaModuleName := "org.enso.logging.service.telemetry",
    version := "0.1",
    commands += WithDebugCommand.withDebug,
    Test / fork := true,
    libraryDependencies ++= slf4jApi ++ Seq(
      "org.netbeans.api"           % "org-openide-util-lookup" % netbeansApiVersion % "provided",
      "junit"                      % "junit"                   % junitVersion       % Test,
      "com.github.sbt"             % "junit-interface"         % junitIfVersion     % Test,
      "org.hamcrest"               % "hamcrest-all"            % hamcrestVersion    % Test,
      "com.fasterxml.jackson.core" % "jackson-core"            % jacksonVersion     % Test,
      "com.fasterxml.jackson.core" % "jackson-annotations"     % jacksonVersion     % Test,
      "com.fasterxml.jackson.core" % "jackson-databind"        % jacksonVersion     % Test
    ),
    Compile / moduleDependencies ++= logbackPkg ++ slf4jApi ++ Seq(
      "org.netbeans.api" % "org-openide-util-lookup" % netbeansApiVersion
    ),
    Compile / internalModuleDependencies ++= Seq(
      (`logging-service` / Compile / exportedModule).value,
      (`logging-service-logback` / Compile / exportedModule).value,
      (`logging-service-common` / Compile / exportedModule).value
    ),
    Test / internalModuleDependencies ++= (Compile / internalModuleDependencies).value ++ Seq(
      (`logging-service-logback` / Compile / exportedModule).value
    )
  )
  .dependsOn(`http-test-helper` % "test->test")
  .dependsOn(`logging-service-common`)
  .dependsOn(testkit % "test->test")

lazy val `logging-service-opensearch` = project
  .in(file("lib/java/logging-service-opensearch"))
  .enablePlugins(JPMSPlugin)
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    javaModuleName := "org.enso.logging.service.opensearch",
    version := "0.1",
    commands += WithDebugCommand.withDebug,
    Test / fork := true,
    libraryDependencies ++= slf4jApi ++ Seq(
      "junit"                      % "junit"            % junitVersion    % Test,
      "com.github.sbt"             % "junit-interface"  % junitIfVersion  % Test,
      "org.hamcrest"               % "hamcrest-all"     % hamcrestVersion % Test,
      "com.fasterxml.jackson.core" % "jackson-core"     % jacksonVersion  % Test,
      "com.fasterxml.jackson.core" % "jackson-databind" % jacksonVersion  % Test
    ),
    Compile / moduleDependencies ++= logbackPkg ++ slf4jApi,
    Compile / internalModuleDependencies ++= Seq(
      (`logging-service` / Compile / exportedModule).value,
      (`logging-service-common` / Compile / exportedModule).value,
      (`logging-service-logback` / Compile / exportedModule).value
    )
  )
  .dependsOn(`logging-service-common`)

lazy val `logging-service-common` = project
  .in(file("lib/java/logging-service-common"))
  .enablePlugins(JPMSPlugin)
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    scalaModuleDependencySetting,
    mixedJavaScalaProjectSetting,
    annotationProcSetting,
    commands += WithDebugCommand.withDebug,
    Test / fork := true,
    libraryDependencies ++= slf4jApi ++ Seq(
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % jsoniterVersion,
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core"   % jsoniterVersion
    ),
    Compile / moduleDependencies ++= logbackPkg ++ slf4jApi,
    Compile / internalModuleDependencies ++= Seq(
      (`scala-libs-wrapper` / Compile / exportedModule).value,
      (`logging-service-logback` / Compile / exportedModule).value
    )
  )
  .dependsOn(`logging-service-logback`)

lazy val `logging-utils-akka` = project
  .in(file("lib/scala/logging-utils-akka"))
  .enablePlugins(JPMSPlugin)
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    scalaModuleDependencySetting,
    annotationProcSetting,
    version := "0.1",
    compileOrder := CompileOrder.ScalaThenJava,
    libraryDependencies ++= slf4jApi ++ Seq(
      "com.typesafe.akka" %% "akka-actor" % akkaVersion
    ),
    Compile / moduleDependencies ++= slf4jApi,
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
    version := "0.1",
    libraryDependencies ++= slf4jApi ++ Seq(
      "junit"          % "junit"           % junitVersion    % Test,
      "com.github.sbt" % "junit-interface" % junitIfVersion  % Test,
      "org.hamcrest"   % "hamcrest-all"    % hamcrestVersion % Test
    ),
    Compile / moduleDependencies ++= slf4jApi,
    Test / fork := true,
    commands += WithDebugCommand.withDebug,
    Test / javaOptions ++= testLogProviderOptions
  )
  .dependsOn(`logging-service-logback` % "test->test")
  .dependsOn(testkit % Test)

lazy val `logging-truffle-connector` = project
  .in(file("lib/scala/logging-truffle-connector"))
  .settings(
    frgaalJavaCompilerSetting,
    version := "0.1",
    libraryDependencies ++= slf4jApi ++ Seq(
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
    libraryDependencies ++= circe ++ scalaReflect ++ slf4jApi ++ Seq(
      "com.typesafe.scala-logging"            %% "scala-logging"         % scalaLoggingVersion,
      "org.typelevel"                         %% "cats-core"             % catsVersion,
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % jsoniterVersion
    ),
    Compile / moduleDependencies ++= scalaLibrary ++ scalaReflect ++ Seq(
      "org.slf4j" % "slf4j-api" % slf4jVersion
    ),
    assembly / assemblyExcludedJars := {
      JPMSUtils.filterModulesFromClasspath(
        (Compile / fullClasspath).value,
        scalaLibrary ++
        scalaReflect ++
        slf4jApi,
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
    },
    assemblyMergeStrategy := { case _ =>
      MergeStrategy.preferProject
    }
  )

lazy val `poi-wrapper` = project
  .in(file("lib/java/poi-wrapper"))
  .settings(
    frgaalJavaCompilerSetting,
    version := "0.1",
    autoScalaLibrary := false,
    libraryDependencies ++= Seq(
      "org.apache.poi" % "poi-ooxml" % poiOoxmlVersion
    ),
    assemblyMergeStrategy := { case _ =>
      MergeStrategy.preferProject
    }
  )

lazy val `runtime-utils` = project
  .in(file("lib/java/runtime-utils"))
  .enablePlugins(JPMSPlugin)
  .settings(
    frgaalJavaCompilerSetting,
    scalaModuleDependencySetting,
    javaModuleName := "org.enso.runtime.utils"
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
    libraryDependencies ++= akka ++ scalaLibrary ++ scalaReflect ++ slf4jApi ++ Seq(
      "org.scala-lang.modules"   %% "scala-parser-combinators" % scalaParserCombinatorsVersion,
      "org.scala-lang.modules"   %% "scala-java8-compat"       % scalaJavaCompatVersion,
      akkaURL                    %% "akka-http"                % akkaHTTPVersion,
      akkaURL                    %% "akka-http-core"           % akkaHTTPVersion,
      akkaURL                    %% "akka-slf4j"               % akkaVersion,
      akkaURL                    %% "akka-parsing"             % akkaHTTPVersion,
      akkaURL                    %% "akka-protobuf-v3"         % akkaVersion,
      akkaURL                    %% "akka-http-spray-json"     % akkaHTTPVersion,
      "com.typesafe"              % "config"                   % typesafeConfigVersion,
      "com.google.protobuf"       % "protobuf-java"            % googleProtobufVersion,
      "io.github.java-diff-utils" % "java-diff-utils"          % javaDiffVersion,
      "org.reactivestreams"       % "reactive-streams"         % reactiveStreamsVersion,
      "io.spray"                 %% "spray-json"               % sprayJsonVersion
    ),
    javaModuleName := "org.enso.akka.wrapper",
    Compile / moduleDependencies ++= slf4jApi ++ Seq(
      "com.google.protobuf" % "protobuf-java"    % googleProtobufVersion,
      "org.reactivestreams" % "reactive-streams" % reactiveStreamsVersion
    ),
    assembly / assemblyExcludedJars := {
      val excludedJars = JPMSUtils.filterModulesFromUpdate(
        update.value,
        scalaLibrary ++ scalaReflect ++ slf4jApi ++ Seq(
          "org.scala-lang.modules"   %% "scala-java8-compat" % scalaJavaCompatVersion,
          "com.typesafe"              % "config"             % typesafeConfigVersion,
          "io.github.java-diff-utils" % "java-diff-utils"    % javaDiffVersion,
          "com.google.protobuf"       % "protobuf-java"      % googleProtobufVersion,
          "org.reactivestreams"       % "reactive-streams"   % reactiveStreamsVersion
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
        scalaLibrary ++ scalaReflect ++
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
        scalaLibrary ++ scalaReflect,
        streams.value.log,
        moduleName.value,
        scalaBinaryVersion.value,
        shouldContainAll = true
      )
      excludedJars
        .map(Attributed.blank)
    },
    Compile / internalModuleDependencies := Seq(
      (`scala-libs-wrapper` / Compile / exportedModule).value
    ),
    Compile / patchModules := {
      val scalaLibs = JPMSUtils.filterModulesFromUpdate(
        update.value,
        scalaLibrary ++
        Seq(
          "dev.zio" %% "zio"                                       % zioVersion,
          "dev.zio" %% "zio-internal-macros"                       % zioVersion,
          "dev.zio" %% "zio-interop-cats"                          % zioInteropCatsVersion,
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
    },
    Runtime / addReads := {
      Map(
        // zio internals tries to access classes from `jdk.unsupported`.
        javaModuleName.value -> Seq(
          "jdk.unsupported"
        )
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
    frgaalJavaCompilerSetting,
    scalaModuleDependencySetting,
    version := "0.1",
    compileOrder := CompileOrder.ScalaThenJava,
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % scalatestVersion % Test
    ),
    Compile / internalModuleDependencies := Seq(
      (`akka-wrapper` / Compile / exportedModuleBin).value,
      (`cli` / Compile / exportedModule).value,
      (`json-rpc-server` / Compile / exportedModule).value,
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
          graalVersion          = graalMavenPackagesVersion,
          javaVersion           = graalVersion,
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
    annotationProcSetting,
    commands += WithDebugCommand.withDebug,
    version := "0.1",
    libraryDependencies ++= Seq(
      "junit"          % "junit"           % junitVersion   % Test,
      "com.github.sbt" % "junit-interface" % junitIfVersion % Test
    ),
    Compile / internalModuleDependencies := Seq(
      (`runtime-parser` / Compile / exportedModule).value,
      (`text-buffer` / Compile / exportedModule).value
    )
  )
  .dependsOn(`runtime-parser`)
  .dependsOn(testkit % Test)
  .dependsOn(`text-buffer`)

lazy val `json-rpc-server` = project
  .in(file("lib/scala/json-rpc-server"))
  .enablePlugins(JPMSPlugin)
  .settings(
    frgaalJavaCompilerSetting,
    scalaModuleDependencySetting,
    compileOrder := CompileOrder.ScalaThenJava,
    libraryDependencies ++= akka ++ logbackTest ++ circe ++ slf4jApi,
    libraryDependencies ++= Seq(
      "io.circe"                   %% "circe-literal"   % circeVersion,
      "com.typesafe.scala-logging" %% "scala-logging"   % scalaLoggingVersion,
      akkaTestkit                   % Test,
      "org.scalatest"              %% "scalatest"       % scalatestVersion      % Test,
      "junit"                       % "junit"           % junitVersion          % Test,
      "com.github.sbt"              % "junit-interface" % junitIfVersion        % Test,
      "org.apache.httpcomponents"   % "httpclient"      % httpComponentsVersion % Test,
      "org.apache.httpcomponents"   % "httpcore"        % httpComponentsVersion % Test,
      "commons-io"                  % "commons-io"      % commonsIoVersion      % Test,
      "org.gnieh"                  %% "diffson-circe"   % diffsonVersion        % Test
    ),
    Compile / moduleDependencies ++= slf4jApi,
    Compile / internalModuleDependencies := Seq(
      (`akka-wrapper` / Compile / exportedModule).value,
      (`scala-libs-wrapper` / Compile / exportedModule).value
    )
  )
  .dependsOn(`runtime-utils` % "test->compile")

// An automatic JPMS module
lazy val testkit = project
  .in(file("lib/scala/testkit"))
  .enablePlugins(JPMSPlugin)
  .settings(
    frgaalJavaCompilerSetting,
    compileOrder := CompileOrder.ScalaThenJava,
    javaModuleName := "org.enso.testkit",
    libraryDependencies ++= logbackPkg ++ slf4jApi ++ Seq(
      "org.apache.commons" % "commons-lang3"   % commonsLangVersion,
      "commons-io"         % "commons-io"      % commonsIoVersion,
      "org.scalatest"     %% "scalatest"       % scalatestVersion,
      "junit"              % "junit"           % junitVersion,
      "com.github.sbt"     % "junit-interface" % junitIfVersion
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
  .dependsOn(`runtime-utils`)

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
  .dependsOn(`polyglot-api`)
  .dependsOn(testkit % Test)

lazy val `ydoc-polyfill` = project
  .in(file("lib/java/ydoc-polyfill"))
  .enablePlugins(JPMSPlugin)
  .configs(Test)
  .settings(
    customFrgaalJavaCompilerSettings("21"),
    javaModuleName := "org.enso.ydoc.polyfill",
    Compile / exportJars := true,
    crossPaths := false,
    autoScalaLibrary := false,
    Test / fork := true,
    commands += WithDebugCommand.withDebug,
    Compile / moduleDependencies ++=
      GraalVM.modules ++ GraalVM.jsPkgs ++ GraalVM.chromeInspectorPkgs ++ helidon ++ slf4jApi,
    Compile / internalModuleDependencies := Seq(
      (`syntax-rust-definition` / Compile / exportedModule).value
    ),
    libraryDependencies ++= logbackTest ++ slf4jApi ++ Seq(
      "org.graalvm.truffle"  % "truffle-api"                 % graalMavenPackagesVersion % "provided",
      "org.graalvm.polyglot" % "inspect-community"           % graalMavenPackagesVersion % "runtime",
      "org.graalvm.polyglot" % "js-community"                % graalMavenPackagesVersion % "runtime",
      "io.helidon.webclient" % "helidon-webclient-websocket" % helidonVersion,
      "io.helidon.webserver" % "helidon-webserver-websocket" % helidonVersion,
      "junit"                % "junit"                       % junitVersion              % Test,
      "com.github.sbt"       % "junit-interface"             % junitIfVersion            % Test
    ),
    libraryDependencies ++= {
      GraalVM.modules ++ GraalVM.jsPkgs
        .map(_ % "provided") ++ GraalVM.chromeInspectorPkgs ++ helidon
    }
  )
  .dependsOn(`syntax-rust-definition`)

lazy val `ydoc-server` = project
  .in(file("lib/java/ydoc-server"))
  .enablePlugins(JPMSPlugin)
  .configs(Test)
  .settings(
    customFrgaalJavaCompilerSettings("21"),
    javaModuleName := "org.enso.ydoc.server",
    Compile / exportJars := true,
    crossPaths := false,
    autoScalaLibrary := false,
    Test / fork := true,
    commands += WithDebugCommand.withDebug,
    Compile / moduleDependencies ++=
      GraalVM.modules ++ GraalVM.jsPkgs ++ GraalVM.chromeInspectorPkgs ++ helidon ++ logbackPkg ++ slf4jApi,
    Compile / internalModuleDependencies := Seq(
      (`syntax-rust-definition` / Compile / exportedModule).value,
      (`ydoc-polyfill` / Compile / exportedModule).value
    ),
    libraryDependencies ++= slf4jApi ++ Seq(
      "org.graalvm.truffle"        % "truffle-api"                 % graalMavenPackagesVersion % "provided",
      "org.graalvm.sdk"            % "nativeimage"                 % graalMavenPackagesVersion % "provided",
      "org.graalvm.polyglot"       % "inspect-community"           % graalMavenPackagesVersion % "runtime",
      "org.graalvm.polyglot"       % "js-community"                % graalMavenPackagesVersion % "runtime",
      "io.helidon.common"          % "helidon-common"              % helidonVersion,
      "io.helidon.webclient"       % "helidon-webclient-websocket" % helidonVersion            % Test,
      "io.helidon.webserver"       % "helidon-webserver-websocket" % helidonVersion            % Test,
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
      val mp        = (Compile / modulePath).value
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
    Compile / resourceGenerators += Def.taskIf {
      if ((Bazel / wasStartedFromBazel).value) {
        val js = (Bazel / ydocServerPolyglotMainJs).value
        val target =
          (Compile / resourceManaged).value / "org" / "enso" / "ydoc" / "server" / "ydoc.cjs"
        IO.createDirectory(target.getParentFile)
        IO.copyFile(js, target)
        Seq(target)
      } else {
        Ydoc.generateJsBundle(
          (ThisBuild / baseDirectory).value,
          baseDirectory.value,
          (Compile / resourceManaged).value,
          streams.value
        )
      }
    }
  )
  .settings(
    NativeImage.smallJdk := None,
    NativeImage.additionalCp := Seq.empty,
    rebuildNativeImage := Def.taskDyn {
      val cLibraryOpts = (Bazel / cLibraryPath).value
        .map(cLib =>
          Seq(
            "-H:CLibraryPath=" + cLib.getAbsolutePath
          )
        )
        .getOrElse(Seq())
      NativeImage
        .buildNativeImage(
          "org.enso.ydoc.server",
          staticOnLinux     = false,
          additionalOptions = cLibraryOpts,
          targetDir         = engineDistributionRoot.value / "component",
          mainClass         = Some("org.enso.ydoc.server.Main"),
          symlink           = false,
          shared            = true
        )
    }.value,
    buildNativeImage := Def.taskDyn {
      NativeImage
        .incrementalNativeImageBuild(
          rebuildNativeImage,
          "org.enso.ydoc.server",
          targetDir = engineDistributionRoot.value / "component",
          shared    = true
        )
    }.value
  )
  .dependsOn(`jvm-interop`)
  .dependsOn(`logging-service-logback`)
  .dependsOn(`ydoc-polyfill`)

lazy val `ydoc-server-registration` = project
  .in(file("lib/java/ydoc-server-registration"))
  .enablePlugins(JPMSPlugin)
  .configs(Test)
  .settings(
    customFrgaalJavaCompilerSettings("21"),
    Compile / exportJars := true,
    crossPaths := false,
    autoScalaLibrary := false,
    Test / fork := true,
    commands += WithDebugCommand.withDebug,
    Compile / moduleDependencies ++=
      GraalVM.modules,
    Compile / internalModuleDependencies := Seq(
      (`engine-runner-common` / Compile / exportedModule).value,
      (`jvm-channel` / Compile / exportedModule).value,
      (`jvm-interop` / Compile / exportedModule).value
    ),
    libraryDependencies ++= Seq(
      "org.graalvm.sdk"      % "nativeimage"       % graalMavenPackagesVersion % "provided",
      "org.graalvm.polyglot" % "inspect-community" % graalMavenPackagesVersion % "runtime",
      "junit"                % "junit"             % junitVersion              % Test,
      "com.github.sbt"       % "junit-interface"   % junitIfVersion            % Test
    ),
    libraryDependencies ++= {
      GraalVM.modules
    }
  )
  .dependsOn(`engine-runner-common`)
  .dependsOn(`jvm-channel`)
  .dependsOn(`jvm-interop`)

lazy val `persistance` = (project in file("lib/java/persistance"))
  .enablePlugins(JPMSPlugin)
  .settings(
    Test / fork := true,
    commands += WithDebugCommand.withDebug,
    frgaalJavaCompilerSetting,
    annotationProcSetting,
    javadocSettings,
    publishLocalSetting,
    autoScalaLibrary := false,
    crossPaths := false,
    Compile / javacOptions := ((Compile / javacOptions).value),
    inConfig(Compile)(truffleRunOptionsSettings),
    libraryDependencies ++= Seq(
      "junit"          % "junit"           % junitVersion   % Test,
      "com.github.sbt" % "junit-interface" % junitIfVersion % Test
    )
  )
  .dependsOn(`persistance-dsl` % Test)

lazy val `persistance-dsl` = (project in file("lib/java/persistance-dsl"))
  .settings(
    frgaalJavaCompilerSetting,
    publishLocalSetting,
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

lazy val `interpreter-dsl` = (project in file("lib/java/interpreter-dsl"))
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
  * its dependencies shall be limited - no JSON & co. please. Also the dependency
  * on sfl4j shall be avoided - rather the module offers a `ContextLoggingConfigurator` seam
  * that must be implemented by some other module.
  */
lazy val `engine-common` = project
  .in(file("engine/common"))
  .enablePlugins(JPMSPlugin)
  .settings(
    frgaalJavaCompilerSetting,
    publishLocalSetting,
    autoScalaLibrary := false,
    crossPaths := false,
    Test / fork := true,
    commands += WithDebugCommand.withDebug,
    Test / envVars ++= distributionEnvironmentOverrides,
    libraryDependencies ++= Seq(
      "org.graalvm.sdk"      % "nativeimage"     % graalMavenPackagesVersion % "provided",
      "org.graalvm.polyglot" % "polyglot"        % graalMavenPackagesVersion % "provided",
      "junit"                % "junit"           % junitVersion              % Test,
      "com.github.sbt"       % "junit-interface" % junitIfVersion            % Test
    ),
    Compile / moduleDependencies ++=
      Seq(
        "org.graalvm.sdk"      % "nativeimage" % graalMavenPackagesVersion,
        "org.graalvm.sdk"      % "word"        % graalMavenPackagesVersion,
        "org.graalvm.polyglot" % "polyglot"    % graalMavenPackagesVersion
      )
  )

lazy val `polyglot-api` = project
  .in(file("engine/polyglot-api"))
  .enablePlugins(JPMSPlugin)
  .settings(
    frgaalJavaCompilerSetting,
    scalaModuleDependencySetting,
    mixedJavaScalaProjectSetting,
    annotationProcSetting,
    Test / fork := true,
    commands += WithDebugCommand.withDebug,
    Test / envVars ++= distributionEnvironmentOverrides,
    Test / javaOptions ++= Seq(
      "-Dpolyglot.engine.WarnInterpreterOnly=false",
      "-Dpolyglotimpl.DisableClassPathIsolation=true"
    ),
    libraryDependencies ++= Seq(
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
      "org.graalvm.sdk"        % "nativeimage"      % graalMavenPackagesVersion,
      "org.graalvm.truffle"    % "truffle-api"      % graalMavenPackagesVersion
    ),
    Compile / internalModuleDependencies := Seq(
      (`editions` / Compile / exportedModule).value,
      (`engine-common` / Compile / exportedModule).value,
      (`logging-utils` / Compile / exportedModule).value,
      (`pkg` / Compile / exportedModule).value,
      (`polyglot-api-macros` / Compile / exportedModule).value,
      (`scala-libs-wrapper` / Compile / exportedModule).value,
      (`text-buffer` / Compile / exportedModule).value
    ),
    GenerateFlatbuffers.flatcVersion := flatbuffersVersion,
    Compile / sourceGenerators += GenerateFlatbuffers.task
  )
  .dependsOn(`engine-common`)
  .dependsOn(`logging-utils`)
  .dependsOn(pkg)
  .dependsOn(`polyglot-api-macros`)
  .dependsOn(testkit % Test)
  .dependsOn(`text-buffer`)

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
    annotationProcSetting,
    libraryDependencies ++= logbackPkg.map(_ % "provided"),
    libraryDependencies ++= akka ++ circe ++ bouncyCastle.map(
      _ % Test
    ) ++ slf4jApi ++ Seq(
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
    Compile / moduleDependencies ++= slf4jApi ++
    (`logging-config` / Compile / moduleDependencies).value ++
    (`logging-utils` / Compile / moduleDependencies).value ++
    (`logging-service` / Compile / moduleDependencies).value ++
    (`logging-service-common` / Compile / moduleDependencies).value ++
    (`logging-service-logback` / Compile / moduleDependencies).value ++
    Seq(
      "org.graalvm.polyglot"   % "polyglot"                % graalMavenPackagesVersion,
      "commons-cli"            % "commons-cli"             % commonsCliVersion,
      "commons-io"             % "commons-io"              % commonsIoVersion,
      "com.google.flatbuffers" % "flatbuffers-java"        % flatbuffersVersion,
      "org.eclipse.jgit"       % "org.eclipse.jgit"        % jgitVersion,
      "org.netbeans.api"       % "org-openide-util-lookup" % netbeansApiVersion
    ),
    Compile / internalModuleDependencies :=
      (`logging-config` / Compile / internalModuleDependencies).value ++
      (`logging-utils` / Compile / internalModuleDependencies).value ++
      (`logging-utils-akka` / Compile / internalModuleDependencies).value ++
      (`logging-service` / Compile / internalModuleDependencies).value ++
      (`logging-service-common` / Compile / internalModuleDependencies).value ++
      (`logging-service-logback` / Compile / internalModuleDependencies).value ++
      Seq(
        (`akka-wrapper` / Compile / exportedModule).value,
        (`zio-wrapper` / Compile / exportedModule).value,
        (`scala-libs-wrapper` / Compile / exportedModule).value,
        (`connected-lock-manager-server` / Compile / exportedModule).value,
        (`language-server-deps-wrapper` / Compile / exportedModule).value,
        (`engine-runner-common` / Compile / exportedModule).value,
        (`ydoc-polyfill` / Compile / exportedModule).value,
        (`engine-common` / Compile / exportedModule).value,
        (`library-manager` / Compile / exportedModule).value,
        (`logging-config` / Compile / exportedModule).value,
        (`logging-utils` / Compile / exportedModule).value,
        (`logging-utils-akka` / Compile / exportedModule).value,
        (`logging-service` / Compile / exportedModule).value,
        (`logging-service-common` / Compile / exportedModule).value,
        (`logging-service-logback` / Compile / exportedModule).value,
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
    libraryDependencies ++= logbackTest ++ Seq(
      "com.google.protobuf"    % "protobuf-java"                % googleProtobufVersion  % Test,
      "org.reactivestreams"    % "reactive-streams"             % reactiveStreamsVersion % Test,
      "org.apache.tika"        % "tika-core"                    % tikaVersion            % Test,
      "com.google.flatbuffers" % "flatbuffers-java"             % flatbuffersVersion     % Test,
      "org.netbeans.api"       % "org-netbeans-modules-sampler" % netbeansApiVersion     % Test,
      "org.apache.commons"     % "commons-lang3"                % commonsLangVersion     % Test,
      "org.apache.commons"     % "commons-compress"             % commonsCompressVersion % Test,
      "org.yaml"               % "snakeyaml"                    % snakeyamlVersion       % Test,
      "com.ibm.icu"            % "icu4j"                        % icuVersion             % Test
    ),
    Test / moduleDependencies := {
      GraalVM.modules ++ GraalVM.langsPkgs ++ logbackPkg ++ helidon ++ bouncyCastle ++ scalaLibrary ++ scalaReflect ++ slf4jApi ++ Seq(
        "org.netbeans.api"       % "org-netbeans-modules-sampler" % netbeansApiVersion,
        "com.google.flatbuffers" % "flatbuffers-java"             % flatbuffersVersion,
        "org.yaml"               % "snakeyaml"                    % snakeyamlVersion,
        "com.typesafe"           % "config"                       % typesafeConfigVersion,
        "org.apache.commons"     % "commons-lang3"                % commonsLangVersion,
        "org.apache.commons"     % "commons-compress"             % commonsCompressVersion,
        "commons-io"             % "commons-io"                   % commonsIoVersion,
        "com.google.protobuf"    % "protobuf-java"                % googleProtobufVersion,
        "org.reactivestreams"    % "reactive-streams"             % reactiveStreamsVersion,
        "org.apache.tika"        % "tika-core"                    % tikaVersion,
        "com.ibm.icu"            % "icu4j"                        % icuVersion,
        "org.netbeans.api"       % "org-openide-util-lookup"      % netbeansApiVersion
      )
    },
    Test / internalModuleDependencies := Seq(
      (Compile / exportedModule).value,
      (`akka-wrapper` / Compile / exportedModule).value,
      (`cli` / Compile / exportedModule).value,
      (`common-polyglot-core-utils` / Compile / exportedModule).value,
      (`connected-lock-manager` / Compile / exportedModule).value,
      (`distribution-manager` / Compile / exportedModule).value,
      (`downloader` / Compile / exportedModule).value,
      (`edition-updater` / Compile / exportedModule).value,
      (`editions` / Compile / exportedModule).value,
      (`engine-common` / Compile / exportedModule).value,
      (`fansi-wrapper` / Compile / exportedModule).value,
      (`interpreter-dsl` / Compile / exportedModule).value,
      (`jvm-channel` / Compile / exportedModule).value,
      (`jvm-interop` / Compile / exportedModule).value,
      (`language-server-deps-wrapper` / Compile / exportedModule).value,
      (`library-manager` / Compile / exportedModule).value,
      (`logging-config` / Compile / exportedModule).value,
      (`logging-service` / Compile / exportedModule).value,
      (`logging-service-logback` / Compile / exportedModule).value,
      (`logging-service-logback` / Test / exportedModule).value,
      (`logging-utils` / Compile / exportedModule).value,
      (`persistance` / Compile / exportedModule).value,
      (`pkg` / Compile / exportedModule).value,
      (`polyglot-api` / Compile / exportedModule).value,
      (`polyglot-api-macros` / Compile / exportedModule).value,
      (`profiling-utils` / Compile / exportedModule).value,
      (`refactoring-utils` / Compile / exportedModule).value,
      (`runtime` / Compile / exportedModule).value,
      (`runtime-compiler` / Compile / exportedModule).value,
      (`runtime-compiler-dump` / Compile / exportedModule).value,
      (`runtime-instrument-common` / Compile / exportedModule).value,
      (`runtime-instrument-id-execution` / Compile / exportedModule).value,
      (`runtime-instrument-repl-debugger` / Compile / exportedModule).value,
      (`runtime-instrument-runtime-server` / Compile / exportedModule).value,
      (`runtime-language-epb` / Compile / exportedModule).value,
      (`runtime-parser` / Compile / exportedModule).value,
      (`runtime-suggestions` / Compile / exportedModule).value,
      (`runtime-utils` / Compile / exportedModule).value,
      (`scala-libs-wrapper` / Compile / exportedModule).value,
      (`scala-yaml` / Compile / exportedModule).value,
      (`semver` / Compile / exportedModule).value,
      (`syntax-rust-definition` / Compile / exportedModule).value,
      (`task-progress-notifications` / Compile / exportedModule).value,
      (`text-buffer` / Compile / exportedModule).value,
      (`version-output` / Compile / exportedModule).value,
      (`ydoc-polyfill` / Compile / exportedModule).value
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
      (`ydoc-polyfill` / javaModuleName).value,
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
  .dependsOn(`connected-lock-manager-server`)
  .dependsOn(`edition-updater`)
  .dependsOn(`engine-runner-common`)
  .dependsOn(filewatcher)
  .dependsOn(`json-rpc-server` % "compile->compile;test->test")
  .dependsOn(`library-manager`)
  .dependsOn(`library-manager` % "test->test")
  .dependsOn(`logging-service`)
  .dependsOn(`logging-service-logback` % Runtime)
  .dependsOn(`logging-service-logback` % "test->test")
  .dependsOn(`logging-service-opensearch` % Runtime)
  .dependsOn(`logging-service-telemetry` % Runtime)
  .dependsOn(`logging-utils-akka`)
  .dependsOn(pkg)
  .dependsOn(`polyglot-api`)
  .dependsOn(`profiling-utils`)
  .dependsOn(`runtime-version-manager` % "test->test")
  .dependsOn(`searcher`)
  .dependsOn(`task-progress-notifications`)
  .dependsOn(testkit % Test)
  .dependsOn(`text-buffer`)
  .dependsOn(`version-output`)
  .dependsOn(`ydoc-polyfill`)

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
      List(
        "Could not determine source for class ",
        "javac: File for type",
        "Could not determine source for class module-info"
      ),
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

/** Ensure that javac compiler generates parameter names for methods, so that these
  * Java methods can be called with named parameters from Scala.
  */
lazy val javaMethodParametersSetting: SettingsDefinition = Seq(
  javacOptions += "-parameters"
)

/** Projects that are published to the local Maven repository via `publishM2` task
  * should incorporate these settings. We need to publish some projects to the local
  * Maven repo, because they are dependencies of some external projects like `enso4igv`.
  * By default, all projects are set `publish / skip := true`.
  */
lazy val publishLocalSetting: SettingsDefinition = Seq(
  version := mavenUploadVersion,
  publish / skip := false,
  packageDoc / publishArtifact := false,
  packageSrc / publishArtifact := false
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
      "-target",
      targetJdk
    )
  )
}

lazy val instrumentationSettings =
  frgaalJavaCompilerSetting ++ annotationProcSetting ++ Seq(
    version := ensoVersion,
    commands += WithDebugCommand.withDebug,
    Compile / javacOptions --= Seq(
      "-source",
      frgaalSourceLevel
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
        "org.graalvm.sdk"      % "word"        % graalMavenPackagesVersion,
        "org.graalvm.sdk"      % "nativeimage" % graalMavenPackagesVersion
      ),
      Compile / internalModuleDependencies := Seq(
        (`jvm-channel` / Compile / exportedModule).value,
        (`jvm-interop` / Compile / exportedModule).value,
        (`runtime-utils` / Compile / exportedModule).value,
        (`ydoc-polyfill` / Compile / exportedModule).value
      )
    )
    .dependsOn(`jvm-interop` % Test)

lazy val `runtime-language-arrow` =
  (project in file("engine/runtime-language-arrow"))
    .enablePlugins(JPMSPlugin)
    .settings(
      crossPaths := false,
      autoScalaLibrary := false,
      javaModuleName := "org.enso.interpreter.arrow",
      inConfig(Compile)(truffleRunOptionsSettings),
      instrumentationSettings,
      customFrgaalJavaCompilerSettings("24"),
      libraryDependencies ++= GraalVM.modules ++ slf4jApi.map(_ % Test) ++ Seq(
        "junit"            % "junit"              % junitVersion       % Test,
        "com.github.sbt"   % "junit-interface"    % junitIfVersion     % Test,
        slf4jNop           % Test,
        "org.apache.arrow" % "arrow-vector"       % apacheArrowVersion % Test,
        "org.apache.arrow" % "arrow-memory-netty" % apacheArrowVersion % Test
      ),
      javaModuleName := "org.enso.interpreter.arrow",
      Compile / moduleDependencies ++= GraalVM.modules,
      Test / internalModuleDependencies += (Compile / exportedModule).value,
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
    // Needed for `java.lang.Foreign`.
    customFrgaalJavaCompilerSettings("24"),
    scalaModuleDependencySetting,
    mixedJavaScalaProjectSetting,
    annotationProcSetting,
    truffleDslSuppressWarnsSetting,
    version := ensoVersion,
    commands += WithDebugCommand.withDebug,
    inConfig(Compile)(truffleRunOptionsSettings),
    libraryDependencies ++= slf4jApi.map(_ % Test) ++ Seq(
      "org.apache.commons"                 % "commons-lang3"           % commonsLangVersion,
      "org.apache.tika"                    % "tika-core"               % tikaVersion,
      "com.lihaoyi"                       %% "fansi"                   % fansiVersion,
      "org.graalvm.polyglot"               % "polyglot"                % graalMavenPackagesVersion % "provided",
      "org.graalvm.sdk"                    % "polyglot-tck"            % graalMavenPackagesVersion % "provided",
      "org.graalvm.truffle"                % "truffle-api"             % graalMavenPackagesVersion % "provided",
      "org.graalvm.truffle"                % "truffle-dsl-processor"   % graalMavenPackagesVersion % "provided",
      "org.graalvm.regex"                  % "regex"                   % graalMavenPackagesVersion % "provided",
      "org.netbeans.api"                   % "org-openide-util-lookup" % netbeansApiVersion        % "provided",
      "org.scalacheck"                    %% "scalacheck"              % scalacheckVersion         % Test,
      "org.scalactic"                     %% "scalactic"               % scalacticVersion          % Test,
      "org.scalatest"                     %% "scalatest"               % scalatestVersion          % Test,
      "junit"                              % "junit"                   % junitVersion              % Test,
      "com.github.sbt"                     % "junit-interface"         % junitIfVersion            % Test,
      "org.hamcrest"                       % "hamcrest-all"            % hamcrestVersion           % Test
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
    Compile / moduleDependencies ++= slf4jApi ++ Seq(
      "org.netbeans.api"     % "org-openide-util-lookup" % netbeansApiVersion,
      "org.apache.tika"      % "tika-core"               % tikaVersion,
      "org.graalvm.truffle"  % "truffle-api"             % graalMavenPackagesVersion,
      "org.graalvm.polyglot" % "polyglot"                % graalMavenPackagesVersion,
      "org.graalvm.sdk"      % "word"                    % graalMavenPackagesVersion,
      "org.graalvm.sdk"      % "nativeimage"             % graalMavenPackagesVersion,
      "com.ibm.icu"          % "icu4j"                   % icuVersion,
      "org.apache.commons"   % "commons-lang3"           % commonsLangVersion
    ),
    Compile / internalModuleDependencies := Seq(
      (`cli` / Compile / exportedModule).value,
      (`common-polyglot-core-utils` / Compile / exportedModule).value,
      (`connected-lock-manager` / Compile / exportedModule).value,
      (`distribution-manager` / Compile / exportedModule).value,
      (`edition-updater` / Compile / exportedModule).value,
      (`editions` / Compile / exportedModule).value,
      (`engine-common` / Compile / exportedModule).value,
      (`fansi-wrapper` / Compile / exportedModule).value,
      (`interpreter-dsl` / Compile / exportedModule).value,
      (`library-manager` / Compile / exportedModule).value,
      (`logging-utils` / Compile / exportedModule).value,
      (`persistance` / Compile / exportedModule).value,
      (`pkg` / Compile / exportedModule).value,
      (`polyglot-api` / Compile / exportedModule).value,
      (`python-resource-provider` / Compile / exportedModule).value,
      (`runtime-compiler` / Compile / exportedModule).value,
      (`runtime-parser` / Compile / exportedModule).value,
      (`runtime-suggestions` / Compile / exportedModule).value,
      (`scala-libs-wrapper` / Compile / exportedModule).value,
      (`syntax-rust-definition` / Compile / exportedModule).value,
      (`text-buffer` / Compile / exportedModule).value,
      (`version-output` / Compile / exportedModule).value
    )
  )
  .settings(
    (Runtime / compile) := (Runtime / compile)
      .dependsOn(`benchmark-java-helpers` / Compile / packageBin)
      .dependsOn(`enso-test-java-helpers` / Compile / packageBin)
      .dependsOn(`exploratory-benchmark-java-helpers` / Compile / packageBin)
      .dependsOn(
        `generic-jdbc-connection-spec-dependencies` / Compile / packageBin
      )
      .dependsOn(`snowflake-test-java-helpers` / Compile / packageBin)
      .dependsOn(`std-aws` / Compile / packageBin)
      .dependsOn(`std-base` / Compile / packageBin)
      .dependsOn(`std-database` / Compile / packageBin)
      .dependsOn(`std-duckdb` / Compile / packageBin)
      .dependsOn(`std-generic-jdbc` / Compile / packageBin)
      .dependsOn(`std-google` / Compile / packageBin)
      .dependsOn(`std-image` / Compile / packageBin)
      .dependsOn(`std-microsoft` / Compile / packageBin)
      .dependsOn(`std-saas` / Compile / packageBin)
      .dependsOn(`std-snowflake` / Compile / packageBin)
      .dependsOn(`std-table` / Compile / packageBin)
      .dependsOn(`std-tableau` / Compile / packageBin)
      .value
  )
  .dependsOn(`common-polyglot-core-utils`)
  .dependsOn(`connected-lock-manager`)
  .dependsOn(`edition-updater`)
  .dependsOn(`interpreter-dsl` % "provided")
  .dependsOn(`library-manager`)
  .dependsOn(`logging-truffle-connector`)
  .dependsOn(`persistance-dsl` % "provided")
  .dependsOn(`polyglot-api`)
  .dependsOn(`python-resource-provider`)
  .dependsOn(`runtime-compiler`)
  .dependsOn(`runtime-suggestions`)
  .dependsOn(testkit % Test)
  .dependsOn(`text-buffer`)

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
      customFrgaalJavaCompilerSettings("24"),
      annotationProcSetting,
      commands += WithDebugCommand.withDebug,
      libraryDependencies ++= GraalVM.modules ++ GraalVM.langsPkgs ++ GraalVM.insightPkgs ++ logbackPkg ++ helidon ++ slf4jApi ++ Seq(
        "org.graalvm.polyglot"       % "polyglot"                     % graalMavenPackagesVersion % "provided",
        "org.graalvm.sdk"            % "polyglot-tck"                 % graalMavenPackagesVersion % "provided",
        "org.graalvm.truffle"        % "truffle-api"                  % graalMavenPackagesVersion % "provided",
        "org.graalvm.truffle"        % "truffle-dsl-processor"        % graalMavenPackagesVersion % "provided",
        "org.graalvm.truffle"        % "truffle-tck"                  % graalMavenPackagesVersion,
        "org.graalvm.truffle"        % "truffle-tck-common"           % graalMavenPackagesVersion,
        "org.graalvm.truffle"        % "truffle-tck-tests"            % graalMavenPackagesVersion,
        "org.netbeans.api"           % "org-openide-util-lookup"      % netbeansApiVersion,
        "org.netbeans.api"           % "org-netbeans-modules-sampler" % netbeansApiVersion,
        "org.scalacheck"            %% "scalacheck"                   % scalacheckVersion         % Test,
        "org.scalactic"             %% "scalactic"                    % scalacticVersion          % Test,
        "org.scalatest"             %% "scalatest"                    % scalatestVersion          % Test,
        "junit"                      % "junit"                        % junitVersion              % Test,
        "com.github.sbt"             % "junit-interface"              % junitIfVersion            % Test,
        "org.hamcrest"               % "hamcrest-all"                 % hamcrestVersion           % Test,
        "com.fasterxml.jackson.core" % "jackson-core"                 % jacksonVersion            % Test,
        "com.fasterxml.jackson.core" % "jackson-annotations"          % jacksonVersion            % Test,
        "com.fasterxml.jackson.core" % "jackson-databind"             % jacksonVersion            % Test,
        "org.yaml"                   % "snakeyaml"                    % snakeyamlVersion
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
        "-Dpolyglot.engine.AllowExperimentalOptions=true",
        "-XX:+HeapDumpOnOutOfMemoryError",
        "-XX:HeapDumpPath=" + (Compile / packageBin).value.getParentFile
      ),
      Test / javaOptions ++= testLogProviderOptions,
      Test / moduleDependencies := {
        GraalVM.modules ++ GraalVM.langsPkgs ++ GraalVM.insightPkgs ++ logbackPkg ++ helidon ++ scalaLibrary ++ scalaReflect ++ slf4jApi ++ Seq(
          "org.apache.commons"     % "commons-lang3"                % commonsLangVersion,
          "org.apache.commons"     % "commons-compress"             % commonsCompressVersion,
          "commons-io"             % "commons-io"                   % commonsIoVersion,
          "org.apache.tika"        % "tika-core"                    % tikaVersion,
          "org.netbeans.api"       % "org-openide-util-lookup"      % netbeansApiVersion,
          "org.netbeans.api"       % "org-netbeans-modules-sampler" % netbeansApiVersion,
          "org.graalvm.sdk"        % "polyglot-tck"                 % graalMavenPackagesVersion,
          "org.graalvm.truffle"    % "truffle-tck"                  % graalMavenPackagesVersion,
          "org.graalvm.truffle"    % "truffle-tck-common"           % graalMavenPackagesVersion,
          "org.graalvm.truffle"    % "truffle-tck-tests"            % graalMavenPackagesVersion,
          "com.ibm.icu"            % "icu4j"                        % icuVersion,
          "com.google.flatbuffers" % "flatbuffers-java"             % flatbuffersVersion,
          "org.yaml"               % "snakeyaml"                    % snakeyamlVersion,
          "com.typesafe"           % "config"                       % typesafeConfigVersion
        )
      },
      Test / internalModuleDependencies := Seq(
        (`cli` / Compile / exportedModule).value,
        (`common-polyglot-core-utils` / Compile / exportedModule).value,
        (`connected-lock-manager` / Compile / exportedModule).value,
        (`distribution-manager` / Compile / exportedModule).value,
        (`downloader` / Compile / exportedModule).value,
        (`edition-updater` / Compile / exportedModule).value,
        (`editions` / Compile / exportedModule).value,
        (`engine-common` / Compile / exportedModule).value,
        (`fansi-wrapper` / Compile / exportedModule).value,
        (`interpreter-dsl` / Compile / exportedModule).value,
        (`jvm-channel` / Compile / exportedModule).value,
        (`jvm-interop` / Compile / exportedModule).value,
        (`library-manager` / Compile / exportedModule).value,
        (`logging-config` / Compile / exportedModule).value,
        (`logging-service` / Compile / exportedModule).value,
        (`logging-service-logback` / Compile / exportedModule).value,
        (`logging-service-logback` / Test / exportedModule).value,
        (`logging-utils` / Compile / exportedModule).value,
        (`persistance` / Compile / exportedModule).value,
        (`pkg` / Compile / exportedModule).value,
        (`polyglot-api` / Compile / exportedModule).value,
        (`polyglot-api-macros` / Compile / exportedModule).value,
        (`profiling-utils` / Compile / exportedModule).value,
        (`refactoring-utils` / Compile / exportedModule).value,
        (`runtime` / Compile / exportedModule).value,
        (`runtime-compiler` / Compile / exportedModule).value,
        (`runtime-compiler-dump` / Compile / exportedModule).value,
        (`runtime-compiler-dump-igv` / Compile / exportedModule).value,
        (`runtime-instrument-common` / Compile / exportedModule).value,
        (`runtime-instrument-id-execution` / Compile / exportedModule).value,
        (`runtime-instrument-repl-debugger` / Compile / exportedModule).value,
        (`runtime-instrument-runtime-server` / Compile / exportedModule).value,
        (`runtime-language-epb` / Compile / exportedModule).value,
        (`runtime-parser` / Compile / exportedModule).value,
        (`runtime-suggestions` / Compile / exportedModule).value,
        (`runtime-test-instruments` / Compile / exportedModule).value,
        (`runtime-utils` / Compile / exportedModule).value,
        (`scala-libs-wrapper` / Compile / exportedModule).value,
        (`scala-yaml` / Compile / exportedModule).value,
        (`semver` / Compile / exportedModule).value,
        (`syntax-rust-definition` / Compile / exportedModule).value,
        (`text-buffer` / Compile / exportedModule).value,
        (`version-output` / Compile / exportedModule).value,
        (`ydoc-polyfill` / Compile / exportedModule).value
      ),
      Test / patchModules := {
        // Patch test-classes into the runtime module. This is standard way to deal with the
        // split package problem in unit tests. For example, Maven's surefire plugin does this.
        val testClassesDir = (Test / productDirectories).value.head
        // Patching with sources is useful for compilation, patching with compiled classes for runtime.
        val javaSrcDir = (Test / javaSource).value
        Map(
          (`runtime` / javaModuleName).value -> Seq(javaSrcDir, testClassesDir)
        )
      },
      Test / addOpens := {
        val compilerModName = (`runtime-compiler` / javaModuleName).value
        // In the tests, we access a private field of org.enso.compiler.pass.PassManager via reflection.
        Map(
          compilerModName + "/org.enso.compiler.pass" -> Seq(
            (`runtime` / javaModuleName).value,
            "ALL-UNNAMED"
          )
        )
      },
      // runtime-integration-tests does not have module descriptor on its own, so we have
      // to explicitly add some modules to the resolution.
      Test / addModules := Seq(
        "scala.library",
        (`runtime` / javaModuleName).value,
        (`runtime-test-instruments` / javaModuleName).value,
        (`ydoc-polyfill` / javaModuleName).value,
        (`runtime-instrument-common` / javaModuleName).value,
        (`runtime-utils` / javaModuleName).value,
        (`text-buffer` / javaModuleName).value,
        (`logging-service-logback` / Test / javaModuleName).value,
        "ch.qos.logback.classic",
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
            (`runtime-utils` / javaModuleName).value,
            (`text-buffer` / javaModuleName).value,
            (`semver` / javaModuleName).value,
            "truffle.tck.tests",
            "org.openide.util.lookup.RELEASE180",
            "ch.qos.logback.classic",
            (`logging-service-logback` / Compile / javaModuleName).value,
            (`logging-service-logback` / Test / javaModuleName).value
          ),
          testInstrumentsModName -> Seq(runtimeModName)
        )
      },
      Test / addExports := {
        // Add necessary exports for IR module dumping to IGV
        // Which is used in the test utils
        val irDumperExports = Map(
          "jdk.graal.compiler/jdk.graal.compiler.graphio" -> Seq(
            (`runtime-compiler-dump-igv` / javaModuleName).value
          )
        )
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
        exports ++ testPkgsExports ++ irDumperExports
      }
    )
    .dependsOn(`connected-lock-manager-server`)
    .dependsOn(`logging-service-logback` % "test->test")
    .dependsOn(`logging-utils` % Test)
    .dependsOn(`runtime`)
    .dependsOn(`runtime-test-instruments`)
    .dependsOn(`runtime-utils` % "test->compile")
    .dependsOn(`test-utils`)
    .dependsOn(testkit % Test)

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
      libraryDependencies ++= GraalVM.modules ++ GraalVM.langsPkgs ++ GraalVM.toolsPkgs ++ helidon ++ logbackPkg ++ slf4jApi ++ Seq(
        "org.openjdk.jmh"     % "jmh-core"                 % jmhVersion,
        "org.openjdk.jmh"     % "jmh-generator-annprocess" % jmhVersion,
        "jakarta.xml.bind"    % "jakarta.xml.bind-api"     % jaxbVersion,
        "com.sun.xml.bind"    % "jaxb-impl"                % jaxbVersion,
        "org.graalvm.truffle" % "truffle-api"              % graalMavenPackagesVersion,
        "org.graalvm.truffle" % "truffle-dsl-processor"    % graalMavenPackagesVersion % "provided",
        slf4jNop,
        "com.typesafe"     % "config"                       % typesafeConfigVersion,
        "org.netbeans.api" % "org-netbeans-modules-sampler" % netbeansApiVersion
      ),
      mainClass :=
        Some("org.enso.interpreter.bench.benchmarks.RuntimeBenchmarksRunner"),
      javacOptions --= Seq(
        "-source",
        frgaalSourceLevel
      ),
      parallelExecution := false,
      Compile / moduleDependencies ++= {
        GraalVM.modules ++ GraalVM.langsPkgs ++ GraalVM.insightPkgs ++ logbackPkg ++ helidon ++ scalaReflect ++ slf4jApi ++ Seq(
          "org.apache.commons" % "commons-lang3"    % commonsLangVersion,
          "org.apache.commons" % "commons-compress" % commonsCompressVersion,
          "commons-io"         % "commons-io"       % commonsIoVersion,
          "org.apache.tika"    % "tika-core"        % tikaVersion,
          slf4jNop,
          "org.netbeans.api"       % "org-openide-util-lookup"      % netbeansApiVersion,
          "org.netbeans.api"       % "org-netbeans-modules-sampler" % netbeansApiVersion,
          "com.ibm.icu"            % "icu4j"                        % icuVersion,
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
        (`benchmarks-common` / Compile / exportedModule).value,
        (`cli` / Compile / exportedModule).value,
        (`common-polyglot-core-utils` / Compile / exportedModule).value,
        (`connected-lock-manager` / Compile / exportedModule).value,
        (`distribution-manager` / Compile / exportedModule).value,
        (`downloader` / Compile / exportedModule).value,
        (`edition-updater` / Compile / exportedModule).value,
        (`editions` / Compile / exportedModule).value,
        (`engine-common` / Compile / exportedModule).value,
        (`fansi-wrapper` / Compile / exportedModule).value,
        (`interpreter-dsl` / Compile / exportedModule).value,
        (`jvm-channel` / Compile / exportedModule).value,
        (`jvm-interop` / Compile / exportedModule).value,
        (`library-manager` / Compile / exportedModule).value,
        (`logging-config` / Compile / exportedModule).value,
        (`logging-service` / Compile / exportedModule).value,
        (`logging-service-logback` / Compile / exportedModule).value,
        (`logging-service-logback` / Test / exportedModule).value,
        (`logging-utils` / Compile / exportedModule).value,
        (`persistance` / Compile / exportedModule).value,
        (`pkg` / Compile / exportedModule).value,
        (`polyglot-api` / Compile / exportedModule).value,
        (`polyglot-api-macros` / Compile / exportedModule).value,
        (`profiling-utils` / Compile / exportedModule).value,
        (`refactoring-utils` / Compile / exportedModule).value,
        (`runtime` / Compile / exportedModule).value,
        (`runtime-compiler` / Compile / exportedModule).value,
        (`runtime-compiler-dump` / Compile / exportedModule).value,
        (`runtime-instrument-common` / Compile / exportedModule).value,
        (`runtime-instrument-id-execution` / Compile / exportedModule).value,
        (`runtime-instrument-repl-debugger` / Compile / exportedModule).value,
        (`runtime-instrument-runtime-server` / Compile / exportedModule).value,
        (`runtime-language-arrow` / Compile / exportedModule).value,
        (`runtime-language-epb` / Compile / exportedModule).value,
        (`runtime-parser` / Compile / exportedModule).value,
        (`runtime-suggestions` / Compile / exportedModule).value,
        (`runtime-utils` / Compile / exportedModule).value,
        (`scala-libs-wrapper` / Compile / exportedModule).value,
        (`scala-yaml` / Compile / exportedModule).value,
        (`semver` / Compile / exportedModule).value,
        (`syntax-rust-definition` / Compile / exportedModule).value,
        (`text-buffer` / Compile / exportedModule).value,
        (`version-output` / Compile / exportedModule).value,
        (`ydoc-polyfill` / Compile / exportedModule).value
      ),
      Compile / addModules := Seq(
        (`runtime` / javaModuleName).value,
        (`benchmarks-common` / javaModuleName).value,
        slf4jNopModule
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
          s"$slf4jNopModule/$slf4jNopModule" -> Seq(slf4jNop.organization)
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
        Def
          .task {
            (Compile / run).toTask(" " + name).value
          }
          .dependsOn(
            buildEngineDistribution
          )
      }.evaluated
    )
    .dependsOn(`benchmarks-common`)
    .dependsOn(`runtime`)
    .dependsOn(`test-utils`)

lazy val `runtime-parser` =
  (project in file("engine/runtime-parser"))
    .enablePlugins(JPMSPlugin)
    .settings(
      scalaModuleDependencySetting,
      mixedJavaScalaProjectSetting,
      javaMethodParametersSetting,
      publishLocalSetting,
      javadocSettings,
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
      ),
      // Java compiler is not able to correctly find all the annotation processors, because
      // one of them is on module-path. To overcome this, we explicitly list all of them here.
      Compile / javacOptions ++= {
        val processorClasses = Seq(
          "org.enso.runtime.parser.processor.IRProcessor",
          "org.enso.persist.impl.PersistableProcessor",
          "org.netbeans.modules.openide.util.ServiceProviderProcessor",
          "org.netbeans.modules.openide.util.NamedServiceProcessor"
        ).mkString(",")
        Seq(
          "-processor",
          processorClasses
        )
      },
      Compile / internalModuleDependencies := Seq(
        (`persistance` / Compile / exportedModule).value,
        (`runtime-parser-dsl` / Compile / exportedModule).value,
        (`runtime-parser-processor` / Compile / exportedModule).value,
        (`syntax-rust-definition` / Compile / exportedModule).value
      )
    )
    .dependsOn(`persistance`)
    .dependsOn(`persistance-dsl` % "provided")
    .dependsOn(`runtime-parser-dsl`)
    .dependsOn(`runtime-parser-processor`)
    .dependsOn(`syntax-rust-definition`)

lazy val `runtime-parser-dsl` =
  (project in file("engine/runtime-parser-dsl"))
    .enablePlugins(JPMSPlugin)
    .settings(
      frgaalJavaCompilerSetting,
      javaMethodParametersSetting,
      publishLocalSetting
    )

lazy val `runtime-parser-processor-tests` =
  (project in file("engine/runtime-parser-processor-tests"))
    .settings(
      inConfig(Compile)(truffleRunOptionsSettings),
      frgaalJavaCompilerSetting,
      javaMethodParametersSetting,
      commands += WithDebugCommand.withDebug,
      annotationProcSetting,
      Compile / javacOptions ++= Seq(
        "-processor",
        "org.enso.runtime.parser.processor.IRProcessor"
      ),
      Test / fork := true,
      libraryDependencies ++= Seq(
        "junit"                      % "junit"           % junitVersion     % Test,
        "com.github.sbt"             % "junit-interface" % junitIfVersion   % Test,
        "org.hamcrest"               % "hamcrest-all"    % hamcrestVersion  % Test,
        "com.google.testing.compile" % "compile-testing" % "0.21.0"         % Test,
        "org.scalatest"             %% "scalatest"       % scalatestVersion % Test
      )
    )
    .dependsOn(`runtime-parser`)
    .dependsOn(`runtime-parser-processor`)

lazy val `runtime-parser-processor` =
  (project in file("engine/runtime-parser-processor"))
    .enablePlugins(JPMSPlugin)
    .configs(Test)
    .settings(
      frgaalJavaCompilerSetting,
      javaMethodParametersSetting,
      publishLocalSetting,
      libraryDependencies ++= Seq(
        "junit"          % "junit"           % junitVersion    % Test,
        "com.github.sbt" % "junit-interface" % junitIfVersion  % Test,
        "org.hamcrest"   % "hamcrest-all"    % hamcrestVersion % Test
      ),
      Compile / internalModuleDependencies := Seq(
        (`runtime-parser-dsl` / Compile / exportedModule).value
      )
    )
    .dependsOn(`runtime-parser-dsl`)

lazy val `runtime-compiler` =
  (project in file("engine/runtime-compiler"))
    .enablePlugins(JPMSPlugin)
    .enablePlugins(PackageListPlugin)
    .settings(
      frgaalJavaCompilerSetting,
      scalaModuleDependencySetting,
      mixedJavaScalaProjectSetting,
      annotationProcSetting,
      mockitoAgentSettings,
      inConfig(Test)(truffleRunOptionsSettings),
      commands += WithDebugCommand.withDebug,
      javaModuleName := "org.enso.runtime.compiler",
      libraryDependencies ++= Seq(
        "junit"                % "junit"                   % junitVersion              % Test,
        "com.github.sbt"       % "junit-interface"         % junitIfVersion            % Test,
        "org.scalatest"       %% "scalatest"               % scalatestVersion          % Test,
        "org.netbeans.api"     % "org-openide-util-lookup" % netbeansApiVersion        % "provided",
        "org.yaml"             % "snakeyaml"               % snakeyamlVersion          % Test,
        "com.typesafe"         % "config"                  % typesafeConfigVersion     % Test,
        "org.graalvm.polyglot" % "polyglot"                % graalMavenPackagesVersion % Test,
        "org.hamcrest"         % "hamcrest-all"            % hamcrestVersion           % Test,
        "com.google.jimfs"     % "jimfs"                   % jimFsVersion              % Test,
        "org.mockito"          % "mockito-core"            % mockitoJavaVersion        % Test,
        "org.mockito"          % "mockito-junit-jupiter"   % mockitoJavaVersion        % Test
      ),
      libraryDependencies ++= logbackPkg.map(_ % Test),
      Compile / moduleDependencies ++= slf4jApi ++ Seq(
        "org.netbeans.api" % "org-openide-util-lookup" % netbeansApiVersion
      ),
      Compile / internalModuleDependencies := Seq(
        (`editions` / Compile / exportedModule).value,
        (`engine-common` / Compile / exportedModule).value,
        (`persistance` / Compile / exportedModule).value,
        (`pkg` / Compile / exportedModule).value,
        (`runtime-compiler-dump` / Compile / exportedModule).value,
        (`runtime-parser` / Compile / exportedModule).value,
        (`scala-libs-wrapper` / Compile / exportedModule).value,
        (`syntax-rust-definition` / Compile / exportedModule).value
      ),
      Test / fork := true,
      Test / javaOptions ++= testLogProviderOptions,
      Test / moduleDependencies ++= {
        (Compile / moduleDependencies).value ++ scalaLibrary ++ scalaReflect ++ logbackPkg ++ Seq(
          "org.apache.commons"   % "commons-compress" % commonsCompressVersion,
          "org.yaml"             % "snakeyaml"        % snakeyamlVersion,
          "com.typesafe"         % "config"           % typesafeConfigVersion,
          "org.graalvm.polyglot" % "polyglot"         % graalMavenPackagesVersion
        )
      },
      Test / internalModuleDependencies := {
        val compileDeps = (Compile / internalModuleDependencies).value
        compileDeps ++ Seq(
          (Compile / exportedModule).value,
          (`runtime-compiler-dump-igv` / Compile / exportedModule).value,
          (`scala-libs-wrapper` / Compile / exportedModule).value,
          (`version-output` / Compile / exportedModule).value,
          (`scala-yaml` / Compile / exportedModule).value,
          (`logging-config` / Compile / exportedModule).value,
          (`logging-service` / Compile / exportedModule).value,
          (`logging-service-logback` / Compile / exportedModule).value,
          (`logging-service-logback` / Test / exportedModule).value,
          (`logging-utils` / Compile / exportedModule).value,
          (`semver` / Compile / exportedModule).value
        )
      },
      Test / addModules := Seq(
        javaModuleName.value
      ),
      Test / patchModules := {
        // Patch test-classes into the runtime module. This is standard way to deal with the
        // split package problem in unit tests. For example, Maven's surefire plugin does this.
        val testClassDir = (Test / productDirectories).value.head
        // Patching with sources is useful for compilation, patching with compiled classes for runtime.
        val javaSrcDir = (Test / javaSource).value
        Map(
          javaModuleName.value -> Seq(
            javaSrcDir,
            testClassDir
          )
        )
      },
      Test / addExports := {
        // Add necessary exports for IR module dumping to IGV
        // Which is used in the test utils
        val irDumperExports = Map(
          "jdk.graal.compiler/jdk.graal.compiler.graphio" -> Seq(
            (`runtime-compiler-dump-igv` / javaModuleName).value
          )
        )

        val testPkgs = (Test / packages).value
        val testPkgsExports = testPkgs.map { pkg =>
          (javaModuleName.value) + "/" + pkg -> Seq("ALL-UNNAMED")
        }.toMap

        testPkgsExports ++ irDumperExports
      },
      Test / addReads := {
        Map(
          javaModuleName.value -> Seq(
            "ALL-UNNAMED",
            "ch.qos.logback.classic",
            (`logging-service-logback` / Compile / javaModuleName).value
          )
        )
      }
    )
    .dependsOn(editions)
    .dependsOn(`engine-common`)
    .dependsOn(`logging-service` % "test->compile")
    .dependsOn(`logging-service-logback` % "test->test")
    .dependsOn(`persistance-dsl` % "provided")
    .dependsOn(pkg)
    .dependsOn(`runtime-compiler-dump`)
    .dependsOn(`runtime-compiler-dump-igv` % "test->compile")
    .dependsOn(`runtime-parser`)

/** This project contains only a single service (interface) definition.
  */
lazy val `runtime-compiler-dump` =
  (project in file("engine/runtime-compiler-dump"))
    .enablePlugins(JPMSPlugin)
    .settings(
      frgaalJavaCompilerSetting,
      scalaModuleDependencySetting,
      Compile / internalModuleDependencies := {
        val transitiveDeps =
          (`runtime-parser` / Compile / internalModuleDependencies).value
        Seq(
          (`runtime-parser` / Compile / exportedModule).value
        ) ++ transitiveDeps
      }
    )
    .dependsOn(`runtime-parser`)

/** This is a standalone project that is not compiled with Frgaal on purpose.
  * It depends on jdk.graal.compiler module, which cannot be included in Frgaal.
  * It includes a service provider for service definition in `runtime-compiler-dump`.
  */
lazy val `runtime-compiler-dump-igv` =
  (project in file("engine/runtime-compiler-dump-igv"))
    .enablePlugins(JPMSPlugin)
    .settings(
      scalaModuleDependencySetting,
      javaModuleName := "org.enso.runtime.compiler.dump.igv",
      Compile / internalModuleDependencies := {
        Seq(
          (`runtime-parser` / Compile / exportedModule).value,
          (`runtime-compiler-dump` / Compile / exportedModule).value
        )
      },
      Compile / moduleDependencies ++= slf4jApi,
      Compile / addExports ++= {
        Map(
          "jdk.graal.compiler/jdk.graal.compiler.graphio" -> Seq(
            javaModuleName.value
          )
        )
      }
    )
    .dependsOn(`runtime-compiler-dump`)

lazy val `runtime-suggestions` =
  (project in file("engine/runtime-suggestions"))
    .enablePlugins(JPMSPlugin)
    .settings(
      frgaalJavaCompilerSetting,
      scalaModuleDependencySetting,
      mixedJavaScalaProjectSetting,
      annotationProcSetting,
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
    .dependsOn(`polyglot-api`)
    .dependsOn(`runtime-compiler`)

lazy val `runtime-instrument-common` =
  (project in file("engine/runtime-instrument-common"))
    .enablePlugins(JPMSPlugin)
    .settings(
      frgaalJavaCompilerSetting,
      scalaModuleDependencySetting,
      mixedJavaScalaProjectSetting,
      inConfig(Compile)(truffleRunOptionsSettings),
      instrumentationSettings,
      Test / javaOptions ++= Seq(
        "-Dpolyglotimpl.DisableClassPathIsolation=true"
      ),
      Test / fork := true,
      Test / envVars ++= distributionEnvironmentOverrides ++ Map(
        "ENSO_TEST_DISABLE_IR_CACHE" -> "false"
      ),
      libraryDependencies ++= Seq(
        "junit"            % "junit"                   % junitVersion       % Test,
        "com.github.sbt"   % "junit-interface"         % junitIfVersion     % Test,
        "org.scalatest"   %% "scalatest"               % scalatestVersion   % Test,
        "org.netbeans.api" % "org-openide-util-lookup" % netbeansApiVersion % Test
      ),
      javaModuleName := "org.enso.runtime.instrument.common",
      Compile / moduleDependencies ++= slf4jApi ++ Seq(
        "org.graalvm.truffle"  % "truffle-api" % graalMavenPackagesVersion,
        "org.graalvm.polyglot" % "polyglot"    % graalMavenPackagesVersion,
        "org.graalvm.sdk"      % "nativeimage" % graalMavenPackagesVersion,
        "org.graalvm.sdk"      % "word"        % graalMavenPackagesVersion
      ),
      Compile / internalModuleDependencies := Seq(
        (`cli` / Compile / exportedModule).value,
        (`connected-lock-manager` / Compile / exportedModule).value,
        (`distribution-manager` / Compile / exportedModule).value,
        (`editions` / Compile / exportedModule).value,
        (`engine-common` / Compile / exportedModule).value,
        (`logging-utils` / Compile / exportedModule).value,
        (`pkg` / Compile / exportedModule).value,
        (`polyglot-api` / Compile / exportedModule).value,
        (`refactoring-utils` / Compile / exportedModule).value,
        (`runtime` / Compile / exportedModule).value,
        (`runtime-compiler` / Compile / exportedModule).value,
        (`runtime-compiler-dump` / Compile / exportedModule).value,
        (`runtime-parser` / Compile / exportedModule).value,
        (`runtime-suggestions` / Compile / exportedModule).value,
        (`runtime-utils` / Compile / exportedModule).value,
        (`scala-libs-wrapper` / Compile / exportedModule).value,
        (`text-buffer` / Compile / exportedModule).value
      )
    )
    .dependsOn(`refactoring-utils`)
    .dependsOn(`runtime` % "compile->compile;runtime->runtime")
    .dependsOn(`runtime-utils`)

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
        "org.graalvm.sdk"      % "word"        % graalMavenPackagesVersion,
        "org.graalvm.sdk"      % "nativeimage" % graalMavenPackagesVersion
      ),
      Compile / internalModuleDependencies := Seq(
        (`polyglot-api` / Compile / exportedModule).value,
        (`runtime` / Compile / exportedModule).value,
        (`runtime-compiler` / Compile / exportedModule).value,
        (`runtime-compiler-dump` / Compile / exportedModule).value
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
        "org.graalvm.sdk"      % "word"        % graalMavenPackagesVersion,
        "org.graalvm.sdk"      % "nativeimage" % graalMavenPackagesVersion
      ),
      Compile / internalModuleDependencies := Seq(
        (`engine-common` / Compile / exportedModule).value,
        (`polyglot-api` / Compile / exportedModule).value,
        (`runtime` / Compile / exportedModule).value,
        (`runtime-compiler` / Compile / exportedModule).value,
        (`runtime-instrument-common` / Compile / exportedModule).value,
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
        "org.graalvm.truffle"  % "truffle-api" % graalMavenPackagesVersion,
        "org.graalvm.polyglot" % "polyglot"    % graalMavenPackagesVersion,
        "org.graalvm.sdk"      % "word"        % graalMavenPackagesVersion,
        "org.graalvm.sdk"      % "nativeimage" % graalMavenPackagesVersion
      ),
      Compile / internalModuleDependencies := Seq(
        (`connected-lock-manager` / Compile / exportedModule).value,
        (`distribution-manager` / Compile / exportedModule).value,
        (`engine-common` / Compile / exportedModule).value,
        (`polyglot-api` / Compile / exportedModule).value,
        (`runtime` / Compile / exportedModule).value,
        (`runtime-compiler` / Compile / exportedModule).value,
        (`runtime-instrument-common` / Compile / exportedModule).value
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
    Compile / moduleDependencies ++= slf4jApi ++ Seq(
      "commons-cli" % "commons-cli" % commonsCliVersion,
      "commons-io"  % "commons-io"  % commonsIoVersion
    ),
    Compile / internalModuleDependencies := Seq(
      (`editions` / Compile / exportedModule).value,
      (`engine-common` / Compile / exportedModule).value,
      (`library-manager` / Compile / exportedModule).value,
      (`logging-utils` / Compile / exportedModule).value,
      (`pkg` / Compile / exportedModule).value,
      (`polyglot-api` / Compile / exportedModule).value
    )
  )
  .dependsOn(`edition-updater`)
  .dependsOn(`library-manager`)
  .dependsOn(`polyglot-api`)
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
    commands += WithDebugCommand.withDebug,
    inConfig(Compile)(truffleRunOptionsSettings),
    libraryDependencies ++= GraalVM.modules ++ GraalVM.toolsPkgs ++ jline ++ Seq(
      "org.graalvm.polyglot"    % "polyglot"                % graalMavenPackagesVersion,
      "org.graalvm.sdk"         % "polyglot-tck"            % graalMavenPackagesVersion % Provided,
      "commons-cli"             % "commons-cli"             % commonsCliVersion,
      "com.monovore"           %% "decline"                 % declineVersion,
      "junit"                   % "junit"                   % junitVersion              % Test,
      "com.github.sbt"          % "junit-interface"         % junitIfVersion            % Test,
      "org.hamcrest"            % "hamcrest-all"            % hamcrestVersion           % Test,
      "org.scala-lang.modules" %% "scala-collection-compat" % scalaCollectionCompatVersion
    ),
    Compile / moduleDependencies ++=
      jline ++
      slf4jApi ++
      Seq(
        "org.graalvm.polyglot" % "polyglot"    % graalMavenPackagesVersion,
        "org.graalvm.sdk"      % "nativeimage" % graalMavenPackagesVersion,
        "org.graalvm.sdk"      % "word"        % graalMavenPackagesVersion,
        "commons-cli"          % "commons-cli" % commonsCliVersion
      ),
    Compile / internalModuleDependencies := Seq(
      (`cli` / Compile / exportedModule).value,
      (`distribution-manager` / Compile / exportedModule).value,
      (`edition-updater` / Compile / exportedModule).value,
      (`editions` / Compile / exportedModule).value,
      (`engine-common` / Compile / exportedModule).value,
      (`engine-runner-common` / Compile / exportedModule).value,
      (`jvm-channel` / Compile / exportedModule).value,
      (`jvm-interop` / Compile / exportedModule).value,
      (`library-manager` / Compile / exportedModule).value,
      (`logging-config` / Compile / exportedModule).value,
      (`logging-utils` / Compile / exportedModule).value,
      (`os-environment` / Compile / exportedModule).value,
      (`pkg` / Compile / exportedModule).value,
      (`polyglot-api` / Compile / exportedModule).value,
      (`process-utils` / Compile / exportedModule).value,
      (`profiling-utils` / Compile / exportedModule).value,
      (`runtime-parser` / Compile / exportedModule).value,
      (`runtime-version-manager` / Compile / exportedModule).value,
      (`semver` / Compile / exportedModule).value,
      (`version-output` / Compile / exportedModule).value,
      (`ydoc-server-registration` / Compile / exportedModule).value
    ),
    // Runtime / modulePath is used as module-path for the native image build.
    Runtime / moduleDependencies :=
      (Compile / moduleDependencies).value ++
      scalaReflect ++
      logbackPkg ++
      Seq(
        "commons-io"             % "commons-io"                   % commonsIoVersion,
        "com.google.flatbuffers" % "flatbuffers-java"             % flatbuffersVersion,
        "com.typesafe"           % "config"                       % typesafeConfigVersion,
        "org.apache.commons"     % "commons-compress"             % commonsCompressVersion,
        "org.apache.tika"        % "tika-core"                    % tikaVersion,
        "org.netbeans.api"       % "org-netbeans-modules-sampler" % netbeansApiVersion,
        "org.yaml"               % "snakeyaml"                    % snakeyamlVersion
      ),
    Runtime / internalModuleDependencies := (Compile / internalModuleDependencies).value ++ Seq(
      (Compile / exportedModule).value,
      (`downloader` / Compile / exportedModule).value,
      (`logging-service` / Compile / exportedModule).value,
      (`logging-service-common` / Compile / exportedModule).value,
      (`logging-service-logback` / Compile / exportedModule).value,
      (`logging-service-opensearch` / Compile / exportedModule).value,
      (`logging-service-telemetry` / Compile / exportedModule).value,
      (persistance / Compile / exportedModule).value,
      (`polyglot-api-macros` / Compile / exportedModule).value,
      (`scala-libs-wrapper` / Compile / exportedModule).value,
      (`scala-yaml` / Compile / exportedModule).value,
      (`syntax-rust-definition` / Compile / exportedModule).value,
      (`text-buffer` / Compile / exportedModule).value
    ),
    Test / moduleDependencies ++= Seq(
      "com.typesafe" % "config" % typesafeConfigVersion
    ),
    run / connectInput := true
  )
  .settings(
    Runtime / javaOptions ++= {
      val runnerCp   = (Runtime / fullClasspath).value
      val runtimeCp  = (`runtime` / Runtime / fullClasspath).value
      val fullCp     = (runnerCp ++ runtimeCp).distinct
      val modulePath = componentModulesPaths.value
      Seq(
        "--enable-native-access=org.graalvm.truffle",
        "--module-path",
        modulePath.map(_.getAbsolutePath).mkString(File.pathSeparator),
        "-m",
        "org.enso.runner/org.enso.runner.Main"
      )
    },
    // For an unknown reason, `Runtime / javaOptions` are appended to `Test / javaOptions`.
    // So we explicitly need to remove the main module option `-m`
    Test / javaOptions := {
      val oldVal = (Test / javaOptions).value
      val idx    = oldVal.indexOf("-m")
      if (idx == -1) {
        throw new IllegalStateException(
          "Expected -m option in Test / javaOptions"
        )
      }
      oldVal.take(idx) ++ oldVal.drop(idx + 2)
    }
  )
  .settings(
    NativeImage.smallJdk := Some(buildSmallJdk.value),
    NativeImage.additionalCp := {
      val runnerDeps =
        (Compile / fullClasspath).value.map(_.data.getAbsolutePath)
      val jvmInteropDeps =
        (`jvm-interop` / Compile / fullClasspath).value.map(
          _.data.getAbsolutePath
        )
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
      def langServer = {
        val log = streams.value.log
        val langServer = (`language-server` / Compile / fullClasspath).value
          .map(_.data.getAbsolutePath)
        val ydocServerRegistration =
          (`ydoc-server-registration` / Compile / fullClasspath).value
            .map(_.data.getAbsolutePath)
        if (GraalVM.EnsoLauncher.disableLanguageServer) {
          log.info(
            s"Skipping language server in native image build as ${GraalVM.EnsoLauncher.VAR_NAME} env variable is ${GraalVM.EnsoLauncher.toString}"
          )
          Seq()
        } else {
          langServer ++ ydocServerRegistration
        }
      }
      val core = (
        runnerDeps ++
          jvmInteropDeps ++
          runtimeDeps ++
          loggingDeps ++
          replDebugInstr ++
          runtimeServerInstr ++
          idExecInstr ++
          langServer ++
          epbLang
      ).distinct
      def stdLibsJars = {
        val log = streams.value.log
        // databaseCp needs to be included in class-path because of
        // the patched SqliteJdbcFeature. It is not enough to include just
        // the jars from `database-polyglot-root`.
        val databaseCp =
          (`std-database` / Compile / fullClasspath).value
            .map(_.data.getAbsolutePath)
        val base =
          `base-polyglot-root`.listFiles("*.jar").map(_.getAbsolutePath())
        if (GraalVM.EnsoLauncher.fast) {
          log.info(
            s"Skipping support for non-Standard.Base libraries in the image build as ${GraalVM.EnsoLauncher.VAR_NAME} env variable is ${GraalVM.EnsoLauncher.toString}"
          )
          base
        } else {
          base ++
          databaseCp ++
          `image-polyglot-root`.listFiles("*.jar").map(_.getAbsolutePath()) ++
          `table-polyglot-root`.listFiles("*.jar").map(_.getAbsolutePath()) ++
          `database-polyglot-root`
            .listFiles("*.jar")
            .map(_.getAbsolutePath()) ++
          `std-tableau-polyglot-root`
            .listFiles("*.jar")
            .map(_.getAbsolutePath()) ++
          `std-duckdb-polyglot-root`
            .listFiles("*.jar")
            .map(_.getAbsolutePath())
        }
      }
      core ++ stdLibsJars ++ extraNITestLibs.value
    },
    extraNITestLibs := Def.taskDyn {
      Def.task {
        Seq[String]()
      }
    }.value,
    buildSmallJdk := {
      val smallJdkDirectory = (target.value / "jdk").getAbsoluteFile()
      SmallJDK.buildSmallJDKForNativeImage(smallJdkDirectory)
      smallJdkDirectory
    },
    rebuildNativeImage := Def
      .taskDyn {
        // Limit max memory limit
        val macArmOnCI =
          sys.env.get("CI").isDefined && Platform.isMacOS && Platform.isArm64
        val maxLimit           = if (macArmOnCI) Some(14336) else Some(15608)
        val areStdlibsIncluded = !GraalVM.EnsoLauncher.fast
        val databaseFeature =
          "org.enso.database.nativeimage.SqliteJdbcPatchedFeature"
        // Features from gax-grpc-2.31.0
        val grpcFeatures =
          "com.google.api.gax.grpc.nativeimage.ProtobufMessageFeature," +
          "com.google.api.gax.grpc.nativeimage.GrpcNettyFeature"
        var features = Seq(
          "org.enso.interpreter.runtime.nativeimage.NativeLibraryFeature"
        )
        if (areStdlibsIncluded) {
          features = features ++ Seq(databaseFeature)
        }
        // heapdump monitoring is not supported on Windows
        val enableHeapDumpOpts =
          if (!GraalVM.EnsoLauncher.release && !Platform.isWindows)
            Seq(
              "--enable-monitoring=heapdump"
            )
          else Seq()
        val linkOpts = if (Platform.isWindows) {
          val ensoTarget = file("target")
          val ensoExp    = file("distribution/bin/enso.exp")
          Seq(
            "-H:NativeLinkerOption=" + ensoExp.getAbsolutePath,
            "-H:TempDirectory=" + ensoTarget.getAbsolutePath,
            "-H:+TraceNativeToolUsage"
          )
        } else {
          Seq()
        }

        val debugOpts =
          if (GraalVM.EnsoLauncher.debug)
            Seq(
              "-g",
              "-O0",
              "-H:+SourceLevelDebug",
              "-H:-DeleteLocalSymbols",
              // you may need to set smallJdk := None to use following flags:
              // "--trace-class-initialization=org.enso.syntax2.Parser",
              // "--diagnostics-mode",
              // "--verbose",
              "-Dnic=nic"
            )
          else Seq()
        val cLibraryOpts = (Bazel / cLibraryPath).value
          .map(cLib =>
            Seq(
              "-H:CLibraryPath=" + cLib.getAbsolutePath
            )
          )
          .getOrElse(Seq())
        val mp = (Runtime / modulePath).value.map(_.getAbsolutePath)
        NativeImage
          .buildNativeImage(
            "enso",
            buildMemoryLimitMegabytes = maxLimit,
            targetDir                 = engineDistributionRoot.value / "bin",
            staticOnLinux             = false,
            // sqlite-jdbc includes `--enable-url-protocols=jar` in its native-image.properites file,
            // which breaks all our class loading. We still want to run `SqliteJdbcFeature` which extracts a proper
            // native library from the jar.
            excludeConfigs = Seq(
              s".*sqlite-jdbc-.*\\.jar,META-INF/native-image/org\\.xerial/sqlite-jdbc/native-image\\.properties",
              ".*gax-grpc-.*\\.jar,META-INF/native-image/com.google.api/gax-grpc/native-image.properties"
            ),
            modulePath = mp,
            additionalOptions = Seq(
              "-Dorg.apache.commons.logging.Log=org.apache.commons.logging.impl.NoOpLog",
              "-H:+AddAllCharsets",
              "-H:+IncludeAllLocales",
              "-R:-InstallSegfaultHandler",
              "-Dorg.sqlite.lib.exportPath=" + (engineDistributionRoot.value / "bin"),
              "--features=" + features.mkString(","),
              // Needed for the NativeLibraryFeature
              "--add-opens=org.graalvm.nativeimage.builder/com.oracle.svm.core.jdk=ALL-UNNAMED",
              // Snowflake uses Apache Arrow (equivalent of #9664 in native-image setup)
              "--add-opens=java.base/java.nio=ALL-UNNAMED",
              // Needed for grpc-gax
              "--add-opens=java.base/java.time=ALL-UNNAMED"
            ) ++ enableHeapDumpOpts ++ debugOpts ++ linkOpts ++ cLibraryOpts,
            mainModule = Some("org.enso.runner"),
            mainClass  = Some("org.enso.runner.Main"),
            initializeAtRuntime = Seq(
              "org.apache",
              "org.openxmlformats",
              "org.jline",
              "zio.internal",
              "zio",
              "org.enso.runner",
              "sun.awt",
              "sun.java2d",
              "sun.font",
              "java.awt",
              "com.sun.imageio",
              "com.sun.jna",
              "com.microsoft",
              "com.azure",
              "akka.http",
              "org.enso.base",
              "org.enso.image",
              "org.enso.logging",
              "org.enso.common.ContextLoggingConfigurator",
              "org.enso.table",
              "org.enso.database",
              "org.enso.tableau",
              "org.eclipse.jgit",
              "com.google",
              "io.grpc",
              "io.netty.util.concurrent.AbstractScheduledEventExecutor",
              "io.netty.resolver.dns",
              "io.opencensus",
              "com.sun.jna",
              "com.tableau.hyperapi",
              "com.typesafe.config.impl.ConfigImpl$EnvVariablesHolder",
              "com.typesafe.config.impl.ConfigImpl$SystemPropertiesHolder",
              // See https://github.com/HarrDevY/native-register-bouncy-castle
              "org.bouncycastle.jcajce.provider.drbg.DRBG$Default",
              "org.bouncycastle.jcajce.provider.drbg.DRBG$NonceAndIV",
              "org.duckdb"
            ),
            initializeAtBuildtime = NativeImage.defaultBuildTimeInitClasses ++
              Seq(
                "org.bouncycastle",
                "org.enso.snowflake.BouncyCastleInitializer",
                "org.enso.interpreter.runtime.nativeimage"
              )
          )
      }
      .dependsOn(NativeImage.additionalCp)
      .dependsOn(NativeImage.smallJdk)
      .dependsOn(
        createEnginePackageNoIndex
      )
      .value,
    buildNativeImage := Def.taskDyn {
      NativeImage
        .incrementalNativeImageBuild(
          rebuildNativeImage,
          "enso",
          targetDir = engineDistributionRoot.value / "bin"
        )
    }.value,
    checkNativeImageSize := Def
      .taskDyn {
        NativeImage.checkNativeImageSize(
          name      = "enso",
          targetDir = engineDistributionRoot.value / "bin"
        )
      }
      .dependsOn(buildNativeImage)
      .value
  )
  .dependsOn(cli)
  .dependsOn(`distribution-manager`)
  .dependsOn(`edition-updater`)
  .dependsOn(`engine-runner-common`)
  .dependsOn(`library-manager`)
  .dependsOn(`logging-service`)
  .dependsOn(`logging-service-logback` % Runtime)
  .dependsOn(`logging-service-opensearch` % Runtime)
  .dependsOn(`logging-service-telemetry` % Runtime)
  .dependsOn(`os-environment`)
  .dependsOn(pkg)
  .dependsOn(`polyglot-api`)
  .dependsOn(`profiling-utils`)
  .dependsOn(`runtime-parser`)
  .dependsOn(`runtime-version-manager`)
  .dependsOn(`version-output`)
  .dependsOn(`ydoc-server-registration`)

lazy val buildSmallJdk =
  taskKey[File]("Build a minimal JDK used for native image generation")

/** Command for building small JDK for the release.
  * Use as `buildSmallJdkForRelease <targetDir>`.
  *
  * If started from bazel, does not take an argument, small jdk will be
  * built in [[BazelSupport.OUT_DIR_PROP]].
  */
ThisBuild / commands += {
  if ((Bazel / wasStartedFromBazel).value) {
    Command.command("buildSmallJdkForRelease") { state =>
      val targetDir = (Bazel / outputDir).value.get
      SmallJDK.buildSmallJDKForRelease(targetDir)
      state.log.info(s"Small JDK built in: $targetDir")
      state
    }
  } else {
    Command.single("buildSmallJdkForRelease") { (state, targetDir) =>
      SmallJDK.buildSmallJDKForRelease(new File(targetDir))
      state.log.info(s"Small JDK built in: $targetDir")
      state
    }
  }
}

lazy val extraNITestLibs =
  taskKey[Seq[String]](
    "List of extra test libraries to be included in Native Image"
  )

lazy val launcher = project
  .in(file("engine/launcher"))
  .enablePlugins(JPMSPlugin)
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    mixedJavaScalaProjectSetting,
    resolvers += Resolver.bintrayRepo("gn0s1s", "releases"),
    commands += WithDebugCommand.withDebug,
    libraryDependencies ++= slf4jApi ++ logbackPkg ++ Seq(
      "com.typesafe.scala-logging" %% "scala-logging"    % scalaLoggingVersion,
      "org.apache.commons"          % "commons-compress" % commonsCompressVersion,
      "org.scalatest"              %% "scalatest"        % scalatestVersion          % Test,
      "org.graalvm.polyglot"        % "polyglot"         % graalMavenPackagesVersion % "provided",
      akkaSLF4J
    ),
    Compile / moduleDependencies := {
      (`logging-utils` / Compile / moduleDependencies).value ++
      (`logging-service` / Compile / moduleDependencies).value ++
      (`logging-service-logback` / Compile / moduleDependencies).value ++
      (`logging-config` / Compile / moduleDependencies).value
    },
    Compile / internalModuleDependencies := {
      (`logging-utils` / Compile / internalModuleDependencies).value ++
      (`logging-service` / Compile / internalModuleDependencies).value ++
      (`logging-service-logback` / Compile / internalModuleDependencies).value ++
      (`logging-config` / Compile / internalModuleDependencies).value ++
      Seq(
        (`logging-utils` / Compile / exportedModule).value,
        (`logging-service` / Compile / exportedModule).value,
        (`logging-service-logback` / Compile / exportedModule).value,
        (`logging-config` / Compile / exportedModule).value
      )
    }
  )
  .settings(
    NativeImage.smallJdk := None,
    NativeImage.additionalCp := Seq.empty,
    rebuildNativeImage := Def
      .taskDyn {
        val mp = (Compile / modulePath).value.map(_.getAbsolutePath)
        NativeImage
          .buildNativeImage(
            "ensoup",
            staticOnLinux = true,
            initializeAtRuntime = Seq(
              "org.jline"
            ),
            additionalOptions = Seq(
              "-Dorg.apache.commons.logging.Log=org.apache.commons.logging.impl.NoOpLog",
              "-H:IncludeResources=.*Main.enso$"
            ),
            modulePath = mp,
            mainClass  = Some("org.enso.launcher.cli.Main")
          )
      }
      .dependsOn(VerifyReflectionSetup.run)
      .value,
    buildNativeImage := NativeImage
      .incrementalNativeImageBuild(
        rebuildNativeImage,
        "ensoup"
      )
      .value,
    cleanFiles += {
      new File("ensoup")
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
  .dependsOn(`distribution-manager` % Test)
  .dependsOn(`logging-service`)
  .dependsOn(`logging-service-logback` % "test->test;runtime->runtime")
  .dependsOn(`logging-utils` % "test->test")
  .dependsOn(pkg)
  .dependsOn(`runtime-version-manager`)
  .dependsOn(`runtime-version-manager` % "test->test")
  .dependsOn(`version-output`)

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
    Compile / moduleDependencies ++= slf4jApi ++ Seq(
      "org.yaml" % "snakeyaml" % snakeyamlVersion
    ),
    Compile / internalModuleDependencies := Seq(
      (`cli` / Compile / exportedModule).value,
      (`logging-utils` / Compile / exportedModule).value,
      (`scala-libs-wrapper` / Compile / exportedModule).value,
      (`scala-yaml` / Compile / exportedModule).value,
      (`semver` / Compile / exportedModule).value
    )
  )
  .dependsOn(cli)
  .dependsOn(editions)
  .dependsOn(`logging-utils`)
  .dependsOn(pkg)

lazy val `test-utils` =
  (project in file("lib/java/test-utils"))
    .settings(
      frgaalJavaCompilerSetting,
      annotationProcSetting,
      libraryDependencies ++= GraalVM.modules,
      libraryDependencies ++= Seq(
        "org.graalvm.truffle" % "truffle-api"           % graalMavenPackagesVersion % "provided",
        "org.graalvm.truffle" % "truffle-dsl-processor" % graalMavenPackagesVersion % "provided",
        "junit"               % "junit"                 % junitVersion
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

lazy val `jvm-channel` =
  project
    .in(file("lib/java/jvm-channel"))
    .enablePlugins(JPMSPlugin)
    .settings(
      customFrgaalJavaCompilerSettings(targetJdk = "24"),
      publishLocalSetting,
      autoScalaLibrary := false,
      crossPaths := false,
      (Test / fork) := true,
      commands += WithDebugCommand.withDebug,
      libraryDependencies ++= Seq(
        "org.graalvm.sdk" % "nativeimage"     % graalMavenPackagesVersion % "provided",
        "org.graalvm.sdk" % "graal-sdk"       % graalMavenPackagesVersion % "provided",
        "junit"           % "junit"           % junitVersion              % Test,
        "com.github.sbt"  % "junit-interface" % junitIfVersion            % Test
      ),
      Compile / moduleDependencies ++= Seq(
        "org.graalvm.sdk"      % "nativeimage" % graalMavenPackagesVersion,
        "org.graalvm.polyglot" % "polyglot"    % graalMavenPackagesVersion,
        "org.graalvm.sdk"      % "word"        % graalMavenPackagesVersion
      ),
      Compile / internalModuleDependencies ++= Seq(
        (`engine-common` / Compile / exportedModule).value,
        (`persistance` / Compile / exportedModule).value
      )
    )
    .dependsOn(`persistance`)
    .dependsOn(`persistance-dsl` % "provided")

lazy val `jvm-interop` =
  project
    .in(file("lib/java/jvm-interop"))
    .enablePlugins(JPMSPlugin)
    .settings(
      annotationProcSetting,
      customFrgaalJavaCompilerSettings("24"),
      // jvm-interop/test has to run with -ea disabled form Truffle.
      // Otherwise Truffle library performs a lot of additional
      // checks and they skew the message counts. Thus enabling -ea
      // only for Enso packages
      inConfig(Compile)(
        Seq(fork := true, javaOptions ++= Seq("-ea:org.enso.jvm..."))
      ),
      publishLocalSetting,
      autoScalaLibrary := false,
      crossPaths := false,
      (Test / fork) := true,
      commands += WithDebugCommand.withDebug,
      libraryDependencies ++= Seq(
        "org.graalvm.truffle" % "truffle-api"           % graalMavenPackagesVersion % "provided",
        "org.graalvm.truffle" % "truffle-dsl-processor" % graalMavenPackagesVersion % "provided",
        "org.graalvm.sdk"     % "graal-sdk"             % graalMavenPackagesVersion % Test,
        "junit"               % "junit"                 % junitVersion              % Test,
        "com.github.sbt"      % "junit-interface"       % junitIfVersion            % Test
      ),
      Compile / moduleDependencies ++= Seq(
        "org.graalvm.truffle"  % "truffle-api" % graalMavenPackagesVersion,
        "org.graalvm.sdk"      % "nativeimage" % graalMavenPackagesVersion,
        "org.graalvm.polyglot" % "polyglot"    % graalMavenPackagesVersion,
        "org.graalvm.sdk"      % "word"        % graalMavenPackagesVersion
      ),
      Compile / internalModuleDependencies ++= Seq(
        (`jvm-channel` / Compile / exportedModule).value,
        (`engine-common` / Compile / exportedModule).value,
        (`persistance` / Compile / exportedModule).value
      )
    )
    .dependsOn(`engine-common`)
    .dependsOn(`jvm-channel`)
    .dependsOn(`persistance-dsl` % "provided")
    .dependsOn(`test-utils` % Test)

lazy val `os-environment-lib` =
  project
    .in(file("lib/java/os-environment-lib"))
    .enablePlugins(JPMSPlugin)
    .settings(
      frgaalJavaCompilerSetting,
      libraryDependencies ++= slf4jApi ++ Seq(
        "org.graalvm.sdk" % "nativeimage"     % graalMavenPackagesVersion % "provided",
        "org.graalvm.sdk" % "graal-sdk"       % graalMavenPackagesVersion % "provided",
        "junit"           % "junit"           % junitVersion              % Test,
        "com.github.sbt"  % "junit-interface" % junitIfVersion            % Test
      ),
      Compile / moduleDependencies ++= slf4jApi ++ Seq(
        "org.graalvm.sdk"      % "nativeimage" % graalMavenPackagesVersion,
        "org.graalvm.polyglot" % "polyglot"    % graalMavenPackagesVersion,
        "org.graalvm.sdk"      % "word"        % graalMavenPackagesVersion
      ),
      Compile / internalModuleDependencies ++= Seq(
        (`engine-common` / Compile / exportedModule).value,
        (`persistance` / Compile / exportedModule).value,
        (`jvm-channel` / Compile / exportedModule).value,
        (`logging-utils` / Compile / exportedModule).value,
        (`logging-config` / Compile / exportedModule).value
      ),
      NativeImage.smallJdk := None,
      NativeImage.additionalCp := {
        val ourDeps = (Test / fullClasspath).value.map(_.data.getAbsolutePath)
        ourDeps
      },
      rebuildNativeImage := Def.taskDyn {
        val targetDir = (Test / target).value
        NativeImage.buildNativeImage(
          "os-environment-lib",
          staticOnLinux = false,
          targetDir     = targetDir,
          symlink       = false,
          mainClass     = Some("org.enso.os.environment.lib.HelloTitle"),
          shared        = true,
          additionalOptions = Seq(
            "-ea",
            "-R:-InstallSegfaultHandler"
          ) ++ (if (GraalVM.EnsoLauncher.debug) {
                  // useful perf & debug switches:
                  Seq(
                    "-g",
                    "-O0",
                    "-H:+SourceLevelDebug",
                    "-H:-DeleteLocalSymbols",
                    "-Dnic=nic"
                  )
                } else {
                  Seq()
                })
        )
      }.value,
      Test / buildNativeImage := Def.taskDyn {
        val targetDir = (Test / target).value
        NativeImage.incrementalNativeImageBuild(
          rebuildNativeImage,
          "os-environment-lib",
          targetDir = targetDir,
          shared    = true
        )
      }.value
    )
    .dependsOn(`engine-common`)
    .dependsOn(`jvm-channel`)
    .dependsOn(`persistance`)
    .dependsOn(`persistance-dsl` % "provided")

lazy val `os-environment` =
  project
    .in(file("lib/java/os-environment"))
    .enablePlugins(JPMSPlugin)
    .settings(
      frgaalJavaCompilerSetting,
      annotationProcSetting,
      libraryDependencies ++= slf4jApi ++ Seq(
        "org.graalvm.sdk" % "nativeimage"     % graalMavenPackagesVersion % "provided",
        "org.graalvm.sdk" % "graal-sdk"       % graalMavenPackagesVersion % "provided",
        "commons-io"      % "commons-io"      % commonsIoVersion,
        "junit"           % "junit"           % junitVersion              % Test,
        "com.github.sbt"  % "junit-interface" % junitIfVersion            % Test
      ),
      Compile / moduleDependencies ++= slf4jApi ++ Seq(
        "commons-io"           % "commons-io"  % commonsIoVersion,
        "org.graalvm.sdk"      % "nativeimage" % graalMavenPackagesVersion,
        "org.graalvm.polyglot" % "polyglot"    % graalMavenPackagesVersion,
        "org.graalvm.sdk"      % "word"        % graalMavenPackagesVersion
      ),
      Compile / internalModuleDependencies ++= Seq(
        (`engine-common` / Compile / exportedModule).value,
        (`persistance` / Compile / exportedModule).value,
        (`jvm-channel` / Compile / exportedModule).value,
        (`logging-utils` / Compile / exportedModule).value,
        (`logging-config` / Compile / exportedModule).value
      ),
      NativeImage.smallJdk := None,
      NativeImage.additionalCp := {
        val ourDeps = (Test / fullClasspath).value.map(_.data.getAbsolutePath)
        ourDeps
      },
      rebuildNativeImage := Def.taskDyn {
        val ignore    = (Test / fullClasspath).value
        val targetDir = (Test / target).value
        NativeImage.buildNativeImage(
          "test-os-env",
          staticOnLinux = false,
          targetDir     = targetDir,
          symlink       = false,
          mainClass     = Some("org.enso.os.environment.TestRunner"),
          additionalOptions = Seq(
            "-ea",
            "--features=org.enso.os.environment.TestCollectorFeature",
            "-R:-InstallSegfaultHandler"
          ) ++ (if (GraalVM.EnsoLauncher.debug) {
                  // useful perf & debug switches:
                  Seq(
                    "-g",
                    "-O0",
                    "-H:+SourceLevelDebug",
                    "-H:-DeleteLocalSymbols",
                    // you may need to set smallJdk := None to use following flags:
                    // "--trace-class-initialization=org.enso.syntax2.Parser",
                    // "--diagnostics-mode",
                    // "--verbose",
                    "-Dnic=nic"
                  )
                } else {
                  Seq()
                })
        )
      }.value,
      Test / buildNativeImage := Def.taskDyn {
        val targetDir = (Test / target).value
        NativeImage.incrementalNativeImageBuild(
          rebuildNativeImage,
          "test-os-env",
          targetDir        = targetDir,
          useTestClassPath = true
        )
      }.value,
      Test / test := Def
        .task {
          val logger    = streams.value.log
          val exeSuffix = if (Platform.isWindows) ".exe" else ""
          val libSuffix =
            if (Platform.isWindows) ".dll"
            else if (Platform.isLinux) ".so"
            else ".dylib"
          val exeFile =
            (Test / target).value / ("test-os-env" + exeSuffix)
          val binPath = exeFile.getAbsolutePath
          val res =
            Process(
              Seq(binPath),
              None,
              "JAVA_TOOL_OPTIONS"  -> "--enable-native-access=org.enso.jvm.channel",
              "OS_ENVIRONMENT_LIB" -> ((`os-environment-lib` / Test / target).value / ("os-environment-lib" + libSuffix)).toString
            ) ! logger
          if (res != 0) {
            logger.error("Some test in os-environment failed")
            throw new TestsFailedException()
          }
        }
        .dependsOn(`os-environment-lib` / Test / buildNativeImage)
        .dependsOn(Test / buildNativeImage)
        .value,
      Test / fork := true
    )
    .dependsOn(`engine-common`)
    .dependsOn(`jvm-channel`)
    .dependsOn(`os-environment-lib` % "test->test")
    .dependsOn(`persistance`)
    .dependsOn(`persistance-dsl` % "provided")

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
      "org.graalvm.polyglot" % "polyglot" % graalMavenPackagesVersion
    ),
    Compile / internalModuleDependencies := Seq(
      (`benchmarks-common` / Compile / exportedModule).value,
      (`engine-common` / Compile / exportedModule).value,
      (`polyglot-api` / Compile / exportedModule).value,
      (`runtime` / Compile / exportedModule).value
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
    libraryDependencies ++= GraalVM.modules ++ GraalVM.langsPkgs ++ GraalVM.toolsPkgs ++ slf4jApi ++ Seq(
      "org.openjdk.jmh"      % "jmh-core"                 % jmhVersion,
      "org.openjdk.jmh"      % "jmh-generator-annprocess" % jmhVersion,
      "org.graalvm.polyglot" % "polyglot"                 % graalMavenPackagesVersion,
      "org.slf4j"            % "slf4j-nop"                % slf4jVersion,
      "org.netbeans.api"     % "org-openide-util-lookup"  % netbeansApiVersion % "provided"
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
      "-J-Dpolyglot.engine.WarnInterpreterOnly=false",
      "-J--sun-misc-unsafe-memory-access=allow"
    ),
    Compile / javaOptions ++= Seq(
      // Force killing of alive threads once a benchmark is finished.
      "-Djmh.shutdownTimeout=0"
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
      (`benchmarks-common` / javaModuleName).value,
      slf4jNopModule
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
        s"$slf4jNopModule/$slf4jNopModule" -> Seq(slf4jNop.organization)
      )
    },
    javaOptions ++= testLogProviderOptions,
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
      Def
        .task {
          (Compile / run).toTask(" " + name).value
        }
        .dependsOn(
          buildEngineDistribution
        )
    }.evaluated
  )
  .dependsOn(`bench-processor`)
  .dependsOn(`benchmark-java-helpers` % "provided")
  .dependsOn(`logging-service-logback`)
  .dependsOn(`profiling-utils`)
  .dependsOn(`runtime-language-arrow`)
  .dependsOn(`std-base` % "provided")
  .dependsOn(`std-table` % "provided")
  .dependsOn(`syntax-rust-definition`)
  .dependsOn(`ydoc-polyfill`)

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
      (`scala-libs-wrapper` / Compile / exportedModule).value,
      (`scala-yaml` / Compile / exportedModule).value,
      (`semver` / Compile / exportedModule).value,
      (`version-output` / Compile / exportedModule).value
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
  .dependsOn(`version-output`)

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
      (`scala-libs-wrapper` / Compile / exportedModule).value,
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
  .dependsOn(`scala-yaml`)
  .dependsOn(testkit % Test)

lazy val downloader = (project in file("lib/scala/downloader"))
  .enablePlugins(JPMSPlugin)
  .settings(
    frgaalJavaCompilerSetting,
    scalaModuleDependencySetting,
    mixedJavaScalaProjectSetting,
    annotationProcSetting,
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
    Compile / moduleDependencies ++= slf4jApi ++ Seq(
      "commons-io"         % "commons-io"       % commonsIoVersion,
      "org.apache.commons" % "commons-compress" % commonsCompressVersion
    ),
    Compile / internalModuleDependencies := Seq(
      (`cli` / Compile / exportedModule).value,
      (`engine-common` / Compile / exportedModule).value,
      (`scala-libs-wrapper` / Compile / exportedModule).value
    )
  )
  .dependsOn(cli)
  .dependsOn(`engine-common`)
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
      (`cli` / Compile / exportedModule).value,
      (`distribution-manager` / Compile / exportedModule).value,
      (`downloader` / Compile / exportedModule).value,
      (`editions` / Compile / exportedModule).value
    )
  )
  .dependsOn(`distribution-manager`)
  .dependsOn(downloader)
  .dependsOn(editions)
  .dependsOn(`library-manager` % "test->test")

lazy val `library-manager` = project
  .in(file("lib/scala/library-manager"))
  .enablePlugins(JPMSPlugin)
  .enablePlugins(PackageListPlugin)
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    scalaModuleDependencySetting,
    compileOrder := CompileOrder.ScalaThenJava, // Note [JPMS Compile order]
    libraryDependencies ++= logbackPkg ++ Seq(
      "com.typesafe"                % "config"        % typesafeConfigVersion,
      "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion,
      "org.graalvm.polyglot"        % "polyglot"      % graalMavenPackagesVersion,
      "org.scalatest"              %% "scalatest"     % scalatestVersion % Test
    ),
    javaModuleName := "org.enso.librarymanager",
    Compile / moduleDependencies ++= slf4jApi ++ Seq(
      "org.yaml" % "snakeyaml" % snakeyamlVersion
    ),
    Compile / internalModuleDependencies := Seq(
      (`cli` / Compile / exportedModule).value,
      (`distribution-manager` / Compile / exportedModule).value,
      (`downloader` / Compile / exportedModule).value,
      (`editions` / Compile / exportedModule).value,
      (`engine-common` / Compile / exportedModule).value,
      (`logging-config` / Compile / exportedModule).value,
      (`logging-service` / Compile / exportedModule).value,
      (`logging-service-logback` / Compile / exportedModule).value,
      (`logging-utils` / Compile / exportedModule).value,
      (`pkg` / Compile / exportedModule).value,
      (`scala-libs-wrapper` / Compile / exportedModule).value,
      (`scala-yaml` / Compile / exportedModule).value,
      (`semver` / Compile / exportedModule).value,
      (`version-output` / Compile / exportedModule).value
    ),
    Compile / moduleDependencies ++= scalaReflect ++ slf4jApi ++ logbackPkg ++ Seq(
      "com.typesafe"         % "config"           % typesafeConfigVersion,
      "commons-io"           % "commons-io"       % commonsIoVersion,
      "org.apache.commons"   % "commons-compress" % commonsCompressVersion,
      "org.graalvm.polyglot" % "polyglot"         % graalMavenPackagesVersion
    ),
    Test / internalModuleDependencies := Seq(
      (Compile / exportedModule).value
    ),
    Test / addModules := Seq(
      javaModuleName.value
    ),
    Test / patchModules := {
      // This is standard way to deal with the
      // split package problem in unit tests. For example, Maven's surefire plugin does this.
      val testClassesDir = (Test / productDirectories).value.head
      val javaSrcDir     = (Test / javaSource).value
      Map(
        javaModuleName.value -> Seq(javaSrcDir, testClassesDir)
      )
    },
    Test / addReads := {
      Map(
        javaModuleName.value -> Seq("ALL-UNNAMED")
      )
    },
    commands += WithDebugCommand.withDebug,
    Test / javaOptions ++= testLogProviderOptions,
    Test / test := (Test / test).tag(simpleLibraryServerTag).value,
    Test / fork := true
  )
  .dependsOn(cli)
  .dependsOn(`distribution-manager`)
  .dependsOn(downloader)
  .dependsOn(editions)
  .dependsOn(`logging-service-logback` % "test->test")
  .dependsOn(`process-utils` % "test->compile")
  .dependsOn(testkit % "test->test")
  .dependsOn(`version-output`) // Note [Default Editions]

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
  .dependsOn(`connected-lock-manager-server` % "test->test")
  .dependsOn(`distribution-manager`)
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
      (`akka-wrapper` / Compile / exportedModule).value,
      (`distribution-manager` / Compile / exportedModule).value,
      (`polyglot-api` / Compile / exportedModule).value,
      (`scala-libs-wrapper` / Compile / exportedModule).value
    )
  )
  .dependsOn(`distribution-manager`)
  .dependsOn(`polyglot-api`)
  .dependsOn(testkit % Test)

lazy val `runtime-version-manager` = project
  .in(file("lib/scala/runtime-version-manager"))
  .enablePlugins(JPMSPlugin)
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    scalaModuleDependencySetting,
    mixedJavaScalaProjectSetting,
    annotationProcSetting,
    resolvers += Resolver.bintrayRepo("gn0s1s", "releases"),
    libraryDependencies ++= Seq(
      "com.typesafe.scala-logging" %% "scala-logging"    % scalaLoggingVersion,
      "org.apache.commons"          % "commons-compress" % commonsCompressVersion,
      "org.apache.tika"             % "tika-core"        % tikaVersion,
      "org.scalatest"              %% "scalatest"        % scalatestVersion % Test
    ),
    Compile / moduleDependencies ++= slf4jApi ++ Seq(
      "org.apache.commons" % "commons-compress" % commonsCompressVersion,
      "org.apache.tika"    % "tika-core"        % tikaVersion,
      "org.yaml"           % "snakeyaml"        % snakeyamlVersion
    ),
    Compile / internalModuleDependencies := Seq(
      (`cli` / Compile / exportedModule).value,
      (`distribution-manager` / Compile / exportedModule).value,
      (`downloader` / Compile / exportedModule).value,
      (`edition-updater` / Compile / exportedModule).value,
      (`editions` / Compile / exportedModule).value,
      (`logging-utils` / Compile / exportedModule).value,
      (`pkg` / Compile / exportedModule).value,
      (`process-utils` / Compile / exportedModule).value,
      (`scala-libs-wrapper` / Compile / exportedModule).value,
      (`scala-yaml` / Compile / exportedModule).value,
      (`semver` / Compile / exportedModule).value,
      (`version-output` / Compile / exportedModule).value
    )
  )
  .settings(
    Test / parallelExecution := false,
    (Test / test) := (Test / test)
      .dependsOn(`locking-test-helper` / assembly)
      .value,
    Test / javaOptions ++= testLogProviderOptions
  )
  .dependsOn(cli)
  .dependsOn(`distribution-manager`)
  .dependsOn(downloader)
  .dependsOn(`edition-updater`)
  .dependsOn(`logging-service-logback` % "test->test")
  .dependsOn(pkg)
  .dependsOn(`process-utils`)
  .dependsOn(testkit % "test->test")
  .dependsOn(`version-output`)

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
  .dependsOn(`runtime-utils`)

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
val `image-native-libs`   = stdLibComponentRoot("Image") / "polyglot" / "lib"
val `generic-jdbc-polyglot-root` =
  stdLibComponentRoot("Generic_JDBC") / "polyglot" / "java"
val `generic-jdbc-native-libs` =
  stdLibComponentRoot("Generic_JDBC") / "polyglot" / "lib"
val `google-polyglot-root` =
  stdLibComponentRoot("Google") / "polyglot" / "java"
val `google-native-libs` =
  stdLibComponentRoot("Google") / "polyglot" / "lib"
val `database-polyglot-root` =
  stdLibComponentRoot("Database") / "polyglot" / "java"
val `database-native-libs` =
  stdLibComponentRoot("Database") / "polyglot" / "lib"
val `std-aws-polyglot-root` =
  stdLibComponentRoot("AWS") / "polyglot" / "java"
val `std-snowflake-polyglot-root` =
  stdLibComponentRoot("Snowflake") / "polyglot" / "java"
val `std-snowflake-native-libs` =
  stdLibComponentRoot("Snowflake") / "polyglot" / "lib"
val `std-microsoft-polyglot-root` =
  stdLibComponentRoot("Microsoft") / "polyglot" / "java"
val `std-microsoft-native-libs` =
  stdLibComponentRoot("Microsoft") / "polyglot" / "lib"
val `std-tableau-polyglot-root` =
  stdLibComponentRoot("Tableau") / "polyglot" / "java"
val `std-tableau-native-libs` =
  stdLibComponentRoot("Tableau") / "polyglot" / "lib"
val `std-saas-polyglot-root` =
  stdLibComponentRoot("Saas") / "polyglot" / "java"
val `std-duckdb-polyglot-root` =
  stdLibComponentRoot("DuckDB") / "polyglot" / "java"
val `std-duckdb-native-libs` =
  stdLibComponentRoot("DuckDB") / "polyglot" / "lib"

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
      "org.graalvm.polyglot"       % "polyglot"         % graalMavenPackagesVersion exclude ("org.graalvm.sdk", "collections"),
      "com.fasterxml.jackson.core" % "jackson-databind" % jacksonVersion,
      "org.slf4j"                  % "slf4j-api"        % slf4jVersion
    ),
    Compile / packageBin := {
      val result = (Compile / packageBin).value
      val _ensureCoreIsCompiled =
        (`common-polyglot-core-utils` / Compile / packageBin).value
      val cacheStoreFactory = streams.value.cacheStoreFactory
      StdBits
        .copyDependencies(
          `base-polyglot-root`,
          Seq("std-base.jar", "common-polyglot-core-utils.jar"),
          ignoreScalaLibrary = true,
          libraryUpdates     = (Compile / update).value,
          logger             = streams.value.log,
          cacheStoreFactory  = cacheStoreFactory,
          unmanagedClasspath = (Compile / unmanagedJars).value
        )
      result
    },
    clean := Def.task {
      val _ = clean.value
      IO.delete(`base-polyglot-root`)
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
      file("test/Base_Tests/polyglot/java/base-test-java-helpers.jar"),
    libraryDependencies ++= Seq(
      "org.graalvm.polyglot" % "polyglot" % graalMavenPackagesVersion % "provided"
    ),
    Compile / packageBin := Def.task {
      val result          = (Compile / packageBin).value
      val primaryLocation = (Compile / packageBin / artifactPath).value
      val secondaryLocations = Seq(
        file("test/Table_Tests/polyglot/java/base-test-java-helpers.jar"),
        file("test/Image_Tests/polyglot/java/base-test-java-helpers.jar")
      )
      secondaryLocations.foreach { target =>
        IO.copyFile(primaryLocation, target)
      }
      result
    }.value
  )
  .dependsOn(`std-base` % "provided")
  .dependsOn(`std-table` % "provided")

lazy val `generic-jdbc-connection-spec-dependencies` = project
  .in(file("lib/java/generic-jdbc-connection-spec-dependencies"))
  .settings(
    frgaalJavaCompilerSetting,
    autoScalaLibrary := false,
    libraryDependencies ++= Seq(
      "org.graalvm.polyglot" % "polyglot" % graalMavenPackagesVersion % "provided",
      "com.h2database"       % "h2"       % h2Version
    ),
    Compile / packageBin := {
      val result            = (Compile / packageBin).value
      val cacheStoreFactory = streams.value.cacheStoreFactory
      StdBits
        .copyDependencies(
          file("test/Generic_JDBC_Tests/polyglot/java/"),
          Seq(),
          ignoreScalaLibrary = true,
          libraryUpdates     = (Compile / update).value,
          logger             = streams.value.log,
          cacheStoreFactory  = cacheStoreFactory,
          unmanagedClasspath = (Compile / unmanagedJars).value
        )
      result
    }
  )
  .dependsOn(`std-base` % "provided")
  .dependsOn(`std-database` % "provided")
  .dependsOn(`std-table` % "provided")

lazy val `snowflake-test-java-helpers` = project
  .in(file("test/Snowflake_Tests/polyglot-sources/snowflake-test-java-helpers"))
  .settings(
    frgaalJavaCompilerSetting,
    autoScalaLibrary := false,
    Compile / packageBin / artifactPath :=
      file("test/Snowflake_Tests/polyglot/java/snowflake-test-helpers.jar")
  )
  .dependsOn(`std-snowflake` % "provided")

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
    customFrgaalJavaCompilerSettings("24"),
    mockitoAgentSettings,
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
      "org.graalvm.polyglot"     % "polyglot"              % graalMavenPackagesVersion % "provided" exclude ("org.graalvm.sdk", "collections"),
      "com.univocity"            % "univocity-parsers"     % univocityParsersVersion,
      "org.apache.poi"           % "poi-ooxml"             % poiOoxmlVersion,
      "org.apache.xmlbeans"      % "xmlbeans"              % xmlbeansVersion,
      "org.antlr"                % "antlr4-runtime"        % antlrVersion,
      "org.apache.logging.log4j" % "log4j"                 % "2.24.3",
      "org.apache.logging.log4j" % "log4j-to-slf4j"        % "2.24.3", // org.apache.poi uses log4j
      "org.graalvm.truffle"      % "truffle-api"           % graalMavenPackagesVersion % Test,
      "junit"                    % "junit"                 % junitVersion              % Test,
      "com.github.sbt"           % "junit-interface"       % junitIfVersion            % Test,
      "org.mockito"              % "mockito-core"          % mockitoJavaVersion        % Test,
      "org.mockito"              % "mockito-junit-jupiter" % mockitoJavaVersion        % Test
    ),
    Compile / unmanagedJars := {
      Seq(
        Attributed.blank((`poi-wrapper` / assembly).value)
      )
    },
    Compile / packageBin := {
      val result            = (Compile / packageBin).value
      val cacheStoreFactory = streams.value.cacheStoreFactory
      StdBits
        .copyDependencies(
          `table-polyglot-root`,
          Seq("std-table.jar"),
          ignoreScalaLibrary = true,
          libraryUpdates     = (Compile / update).value,
          unmanagedClasspath = (Compile / unmanagedJars).value,
          ignoreDependenciesByModuleID = Some(
            Seq(
              "org.apache.poi" % "poi"            % poiOoxmlVersion,
              "org.apache.poi" % "poi-ooxml"      % poiOoxmlVersion,
              "org.apache.poi" % "poi-ooxml-lite" % poiOoxmlVersion
            )
          ),
          logger            = streams.value.log,
          cacheStoreFactory = cacheStoreFactory
        )
      result
    },
    clean := Def.task {
      val _ = clean.value
      IO.delete(`table-polyglot-root`)
    }.value
  )
  .dependsOn(`poi-wrapper`)
  .dependsOn(`std-base` % "provided")

lazy val `std-tests` = project
  .in(file("std-bits") / "tests")
  .configs(Test)
  .settings(
    frgaalJavaCompilerSetting,
    commands += WithDebugCommand.withDebug,
    Test / fork := true,
    Test / javaOptions ++= Seq(
      "-ea"
    ),
    autoScalaLibrary := false,
    Compile / compile / compileInputs := (Compile / compile / compileInputs)
      .dependsOn(SPIHelpers.ensureSPIConsistency)
      .value,
    libraryDependencies ++= Seq(
      "junit"          % "junit"           % junitVersion   % Test,
      "com.github.sbt" % "junit-interface" % junitIfVersion % Test
    )
  )
  .dependsOn(`std-base`)
  .dependsOn(`std-table`)
  .dependsOn(`runtime-language-arrow`)
  .dependsOn(`test-utils`)

lazy val `opencv-wrapper` = project
  .in(file("lib/java/opencv-wrapper"))
  .enablePlugins(JarExtractPlugin)
  .settings(
    libraryDependencies ++= Seq(
      "org.openpnp" % "opencv" % opencvVersion
    ),
    inputJar := "org.openpnp" % "opencv" % opencvVersion,
    jarExtractor := JarExtractor(
      "nu/pattern/opencv/linux/x86_64/*.so"    -> PolyglotLib(LinuxAMD64),
      "nu/pattern/opencv/osx/ARMv8/*.dylib"    -> PolyglotLib(MacOSArm64),
      "nu/pattern/opencv/windows/x86_64/*.dll" -> PolyglotLib(WindowsAMD64),
      "nu/pattern/*.class"                     -> CopyToOutputJar,
      "META-INF/**"                            -> CopyToOutputJar,
      "org/**"                                 -> CopyToOutputJar
    )
  )

lazy val `jna-wrapper-extracted` = project
  .in(file("lib/java/jna-wrapper-extracted"))
  .enablePlugins(JarExtractPlugin)
  .settings(
    inputJarResolved := {
      (`jna-wrapper` / Compile / exportedModuleBin).value
    },
    jarExtractor := JarExtractor(
      "com/sun/jna/linux-x86-64/libjnidispatch.so" -> PolyglotLib(LinuxAMD64),
      "com/sun/jna/win32-x86-64/jnidispatch.dll"   -> PolyglotLib(WindowsAMD64),
      "com/sun/jna/darwin-aarch64/libjnidispatch.jnilib" -> PolyglotLib(
        MacOSArm64
      ),
      "com/**/*.class"       -> CopyToOutputJar,
      "module-info.class"    -> CopyToOutputJar,
      "META-INF/MANIFEST.MF" -> CopyToOutputJar,
      "META-INF/LICENSE"     -> CopyToOutputJar,
      "META-INF/LGPL2.1"     -> CopyToOutputJar,
      "META-INF/AL2.0"       -> CopyToOutputJar
    )
  )
  .dependsOn(`jna-wrapper`)

lazy val `netty-tc-native-wrapper` = project
  .in(file("lib/java/tc-native-wrapper"))
  .enablePlugins(JarExtractPlugin)
  .settings(
    libraryDependencies ++= Seq(
      "io.netty" % "netty-tcnative-boringssl-static" % "2.0.70.Final"
    ),
    // We have to explicitly select correct jar based on the current platform.
    inputJarResolved := {
      val tcNativeJars = JPMSUtils.filterModulesFromUpdate(
        updateReport = (Compile / update).value,
        modules = Seq(
          "io.netty" % "netty-tcnative-boringssl-static" % "2.0.70.Final"
        ),
        log                = streams.value.log,
        projName           = moduleName.value,
        scalaBinaryVersion = scalaBinaryVersion.value,
        shouldContainAll   = true
      )
      // tcNativeJar has name like:
      // "netty-tcnative-boringssl-static-2.0.70.Final-linux-x86_64.jar"
      // It contains just a single native library
      def isExpectedTcNativeJarName(name: String): Boolean = {
        name.contains(Platform.arch().replace("aarch64", "aarch_64")) &&
        name.contains(Platform.osName())
      }
      val tcNativeJar = tcNativeJars.filter { jar =>
        isExpectedTcNativeJarName(jar.getName)
      }
      if (tcNativeJar.size != 1) {
        throw new IllegalStateException(
          s"Expected exactly one tc native jar for ${Platform.osName()}-${Platform
            .arch()}, but found: ${tcNativeJar.mkString(", ")}"
        )
      }
      tcNativeJar.head
    },
    jarExtractor := JarExtractor(
      "META-INF/native/libnetty_tcnative_osx_aarch_64.jnilib" -> PolyglotLib(
        MacOSArm64
      ),
      "META-INF/native/netty_tcnative_windows_x86_64.dll" -> PolyglotLib(
        WindowsAMD64
      ),
      "META-INF/native/libnetty_tcnative_linux_x86_64.so" -> PolyglotLib(
        LinuxAMD64
      ),
      "META-INF/license/*"   -> CopyToOutputJar,
      "META-INF/maven/**"    -> CopyToOutputJar,
      "META-INF/versions/**" -> CopyToOutputJar
    )
  )

// Native libs only for Linux.
// For other platforms, the output directory should be empty.
lazy val `netty-epoll-native-wrapper` = project
  .in(file("lib/java/epoll-native-wrapper"))
  .enablePlugins(JarExtractPlugin)
  .settings(
    libraryDependencies ++= Seq(
      "io.netty" % "netty-transport-native-epoll" % "4.1.118.Final"
    ),
    inputJar := "io.netty" % "netty-transport-native-epoll" % "4.1.118.Final",
    jarExtractor := JarExtractor(
      "**/libnetty_transport_native_epoll_x86_64.so" -> PolyglotLib(LinuxAMD64)
    )
  )

// Native lib only for Mac
// For other platforms, the output directory should be empty.
lazy val `netty-resolver-dns-native-macos-wrapper` = project
  .in(file("lib/java/resolver-dns-native-wrapper"))
  .enablePlugins(JarExtractPlugin)
  .settings(
    libraryDependencies ++= Seq(
      "io.netty"  % "netty-resolver-dns-native-macos" % "4.1.118.Final",
      ("io.netty" % "netty-resolver-dns-native-macos" % "4.1.118.Final")
        .classifier("osx-aarch_64")
    ),
    // Correct jar needs to be selected manually, because filtering modules does not
    // normally work for classifiers.
    inputJarResolved := {
      val nettyResolverNativeJars = JPMSUtils.filterModulesFromUpdate(
        updateReport = (Compile / update).value,
        modules = Seq(
          ("io.netty" % "netty-resolver-dns-native-macos" % "4.1.118.Final")
            .classifier("osx-aarch_64")
        ),
        log                = streams.value.log,
        projName           = moduleName.value,
        scalaBinaryVersion = scalaBinaryVersion.value,
        shouldContainAll   = true
      )
      val nativeJar = nettyResolverNativeJars.filter { jar =>
        jar.name.contains("osx-aarch_64")
      }
      if (nativeJar.size != 1) {
        throw new IllegalStateException(
          s"Expected exactly one netty resolver dns native jar for macos-aarch_64, but found: ${nativeJar
            .mkString(", ")}"
        )
      }
      nativeJar.head
    },
    jarExtractor := JarExtractor(
      "META-INF/native/libnetty_resolver_dns_native_macos_aarch_64.jnilib" -> PolyglotLib(
        MacOSArm64
      )
    )
  )

lazy val `tableau-wrapper` = project
  .in(file("lib/java/tableau-wrapper"))
  .enablePlugins(JarExtractPlugin)
  .settings(
    inputJarResolved := {
      val tableauJars =
        (LocalProject("std-tableau") / Compile / unmanagedJars).value
          .map(_.data)
      val tableauSuffixInJar = s"tableauhyperapi-${StdBits.plainOsName()}"
      tableauJars.filter(f => f.getName.contains(tableauSuffixInJar)).head
    },
    jarExtractor := JarExtractor(
      "darwin-aarch64/libtableauhyperapi.dylib" -> PolyglotLib(MacOSArm64),
      "linux-x86-64/libtableauhyperapi.so"      -> PolyglotLib(LinuxAMD64),
      "win32-x86-64/tableauhyperapi.dll"        -> PolyglotLib(WindowsAMD64)
    )
  )

lazy val `grpc-wrapper` = project
  .in(file("lib/java/grpc-wrapper"))
  .enablePlugins(JarExtractPlugin)
  .settings(
    libraryDependencies ++= Seq(
      "io.grpc" % "grpc-netty-shaded" % grpcVersion
    ),
    inputJar := "io.grpc" % "grpc-netty-shaded" % grpcVersion,
    jarExtractor := JarExtractor(
      "META-INF/native/libio_grpc_netty_shaded_netty_tcnative_linux_x86_64.so" -> PolyglotLib(
        LinuxAMD64
      ),
      "META-INF/native/libio_grpc_netty_shaded_netty_transport_native_epoll_x86_64.so" -> PolyglotLib(
        LinuxAMD64
      ),
      "META-INF/native/libio_grpc_netty_shaded_netty_tcnative_osx_aarch_64.jnilib" -> PolyglotLib(
        MacOSArm64
      ),
      "META-INF/native/io_grpc_netty_shaded_netty_tcnative_windows_x86_64.dll" -> PolyglotLib(
        WindowsAMD64
      ),
      "META-INF/MANIFEST.MF"                  -> CopyToOutputJar,
      "META-INF/LICENSE.txt"                  -> CopyToOutputJar,
      "META-INF/NOTICE.txt"                   -> CopyToOutputJar,
      "META-INF/io.netty.versions.properties" -> CopyToOutputJar,
      "META-INF/services/**"                  -> CopyToOutputJar,
      "META-INF/license/**"                   -> CopyToOutputJar,
      "io/**/*.class"                         -> CopyToOutputJar
    )
  )

/** Same as `grpc-wrapper`, but uses an older version of gRPC.
  */
lazy val `grpc-wrapper-older` = project
  .in(file("lib/java/grpc-wrapper-older"))
  .enablePlugins(JarExtractPlugin)
  .settings(
    libraryDependencies ++= Seq(
      "io.grpc" % "grpc-netty-shaded" % "1.60.0"
    ),
    inputJar := "io.grpc" % "grpc-netty-shaded" % "1.60.0",
    jarExtractor := (`grpc-wrapper` / jarExtractor).value
  )

lazy val `jline-wrapper` = project
  .in(file("lib/java/jline-wrapper"))
  .enablePlugins(JarExtractPlugin)
  .settings(
    libraryDependencies ++= Seq(
      "org.jline" % "jline-native" % jlineVersion
    ),
    inputJar := "org.jline" % "jline-native" % jlineVersion,
    jarExtractor := JarExtractor(
      "org/jline/nativ/Linux/x86_64/libjlinenative.so" -> PolyglotLib(
        LinuxAMD64
      ),
      "org/jline/nativ/Mac/arm64/libjlinenative.jnilib" -> PolyglotLib(
        MacOSArm64
      ),
      "org/jline/nativ/Windows/x86_64/jlinenative.dll" -> PolyglotLib(
        WindowsAMD64
      ),
      "org/jline/nativ/*.class"  -> CopyToOutputJar,
      "META-INF/MANIFEST.MF"     -> CopyToOutputJar,
      "META-INF/maven/**"        -> CopyToOutputJar,
      "META-INF/native-image/**" -> CopyToOutputJar
    )
  )

lazy val `conscrypt-wrapper` = project
  .in(file("lib/java/constrypt-wrapper"))
  .enablePlugins(JarExtractPlugin)
  .settings(
    libraryDependencies := Seq(
      "org.conscrypt" % "conscrypt-openjdk-uber" % "2.5.2"
    ),
    inputJar := "org.conscrypt" % "conscrypt-openjdk-uber" % "2.5.2",
    jarExtractor := JarExtractor(
      "META-INF/native/libconscrypt_openjdk_jni-linux-x86_64.so" -> PolyglotLib(
        LinuxAMD64
      ),
      "META-INF/native/conscrypt_openjdk_jni-windows-x86_64.dll" -> PolyglotLib(
        WindowsAMD64
      ),
      "META-INF/MANIFEST.MF"               -> CopyToOutputJar,
      "org/conscrypt/conscrypt.properties" -> CopyToOutputJar,
      "org/**/*.class"                     -> CopyToOutputJar
    )
  )

lazy val `sqlite-wrapper` = project
  .in(file("lib/java/sqlite-wrapper"))
  .enablePlugins(JarExtractPlugin)
  .settings(
    libraryDependencies ++= Seq(
      "org.xerial" % "sqlite-jdbc" % sqliteVersion
    ),
    inputJar := "org.xerial" % "sqlite-jdbc" % sqliteVersion,
    jarExtractor := JarExtractor(
      "org/sqlite/native/Linux/x86_64/libsqlitejdbc.so" -> PolyglotLib(
        LinuxAMD64
      ),
      "org/sqlite/native/Mac/aarch64/libsqlitejdbc.dylib" -> PolyglotLib(
        MacOSArm64
      ),
      "org/sqlite/native/Windows/x86_64/sqlitejdbc.dll" -> PolyglotLib(
        WindowsAMD64
      ),
      "META-INF/MANIFEST.MF"                  -> CopyToOutputJar,
      "META-INF/maven/**"                     -> CopyToOutputJar,
      "META-INF/services/**"                  -> CopyToOutputJar,
      "META-INF/versions/9/module-info.class" -> CopyToOutputJar,
      "9/module-info.class"                   -> CopyToOutputJar,
      "org/**/*.class"                        -> CopyToOutputJar,
      "sqlite-jdbc.properties"                -> CopyToOutputJar
    )
  )

lazy val `duckdb-wrapper` = project
  .in(file("lib/java/duckdb-wrapper"))
  .enablePlugins(JarExtractPlugin)
  .settings(
    frgaalJavaCompilerSetting,
    autoScalaLibrary := false,
    libraryDependencies ++= Seq(
      "org.duckdb" % "duckdb_jdbc" % duckdbVersion
    ),
    version := "0.1",
    jarExtractor := JarExtractor(
      "libduckdb_java.so_linux_amd64"   -> PolyglotLib(LinuxAMD64),
      "libduckdb_java.so_osx_universal" -> PolyglotLib(MacOSArm64),
      "libduckdb_java.so_windows_amd64" -> PolyglotLib(WindowsAMD64),
      "META-INF/**"                     -> CopyToOutputJar,
      "org/**/*.class"                  -> CopyToOutputJar
    ),
    inputJarResolved := assembly.value,
    assemblyMergeStrategy := { case _ =>
      MergeStrategy.preferProject
    }
  )

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
      "org.graalvm.polyglot" % "polyglot" % graalMavenPackagesVersion % "provided",
      "org.openpnp"          % "opencv"   % opencvVersion
    ),
    Compile / packageBin := {
      val logger            = streams.value.log
      val cacheStoreFactory = streams.value.cacheStoreFactory
      val stdImageJar       = (Compile / packageBin).value
      StdBits
        .copyDependencies(
          `image-polyglot-root`,
          Seq("std-image.jar"),
          ignoreScalaLibrary = true,
          ignoreDependenciesByModuleID =
            Some(Seq("org.openpnp" % "opencv" % opencvVersion)),
          libraryUpdates     = (Compile / update).value,
          logger             = logger,
          cacheStoreFactory  = cacheStoreFactory,
          unmanagedClasspath = (Compile / unmanagedJars).value,
          polyglotLibDir     = Some(`image-native-libs`),
          extractedNativeLibsDirs =
            Seq((`opencv-wrapper` / extractedFilesDir).value),
          extraJars = Seq((`opencv-wrapper` / thinJarOutput).value)
        )
      stdImageJar
    },
    clean := Def.task {
      val _ = clean.value
      IO.delete(`image-polyglot-root`)
      IO.delete(`image-native-libs`)
    }.value
  )
  .dependsOn(`std-base` % "provided")

lazy val `std-generic-jdbc` = project
  .in(file("std-bits") / "generic-jdbc")
  .settings(
    frgaalJavaCompilerSetting,
    autoScalaLibrary := false,
    Compile / compile / compileInputs := (Compile / compile / compileInputs)
      .dependsOn(SPIHelpers.ensureSPIConsistency)
      .value,
    Compile / packageBin / artifactPath :=
      `generic-jdbc-polyglot-root` / "std-generic-jdbc.jar",
    libraryDependencies ++= Seq(
      "org.graalvm.polyglot" % "polyglot" % graalMavenPackagesVersion % "provided"
    ),
    Compile / packageBin := {
      val result            = (Compile / packageBin).value
      val cacheStoreFactory = streams.value.cacheStoreFactory
      StdBits
        .copyDependencies(
          `generic-jdbc-polyglot-root`,
          Seq("std-generic-jdbc.jar"),
          ignoreScalaLibrary = true,
          libraryUpdates     = (Compile / update).value,
          unmanagedClasspath = (Compile / unmanagedClasspath).value,
          logger             = streams.value.log,
          cacheStoreFactory
        )
      result
    },
    clean := Def.task {
      val _ = clean.value
      IO.delete(`generic-jdbc-polyglot-root`)
    }.value
  )
  .dependsOn(`std-base` % "provided")
  .dependsOn(`std-database` % "provided")
  .dependsOn(`std-table` % "provided")

lazy val `std-google` = project
  .in(file("std-bits") / "google")
  .settings(
    frgaalJavaCompilerSetting,
    autoScalaLibrary := false,
    Compile / compile / compileInputs := (Compile / compile / compileInputs)
      .dependsOn(SPIHelpers.ensureSPIConsistency)
      .value,
    Compile / packageBin / artifactPath :=
      `google-polyglot-root` / "std-google.jar",
    libraryDependencies ++= Seq(
      "com.google.api-client" % "google-api-client"          % googleApiClientVersion exclude ("com.google.code.findbugs", "jsr305"),
      "com.google.apis"       % "google-api-services-sheets" % googleApiServicesSheetsVersion exclude ("com.google.code.findbugs", "jsr305"),
      "com.google.analytics"  % "google-analytics-admin"     % googleAnalyticsAdminVersion exclude ("com.google.code.findbugs", "jsr305") exclude ("io.grpc", "grpc-xds"),
      "com.google.analytics"  % "google-analytics-data"      % googleAnalyticsDataVersion exclude ("com.google.code.findbugs", "jsr305") exclude ("io.grpc", "grpc-xds"),
      "io.grpc"               % "grpc-netty-shaded"          % grpcVersion exclude ("com.google.code.findbugs", "jsr305")
    ),
    // Extract native libraries from grpc-netty-shaded-***.jar, and put them under
    // Standard/Google/polyglot/lib directory. The minimized jar will
    // be put under Standard/Google/polyglot/java directory.
    Compile / packageBin := {
      val logger            = streams.value.log
      val cacheStoreFactory = streams.value.cacheStoreFactory
      val stdGoogleJar      = (Compile / packageBin).value
      StdBits
        .copyDependencies(
          `google-polyglot-root`,
          Seq("std-google.jar"),
          ignoreScalaLibrary = true,
          ignoreDependencyIncludeTransitive =
            Some(s"grpc-netty-shaded-${grpcVersion}"),
          ignoreDependenciesByModuleID = Some(
            Seq(
              "org.conscrypt" % "conscrypt-openjdk-uber" % "2.5.2"
            )
          ),
          libraryUpdates     = (Compile / update).value,
          logger             = streams.value.log,
          cacheStoreFactory  = cacheStoreFactory,
          unmanagedClasspath = (Compile / unmanagedJars).value,
          polyglotLibDir     = Some(`google-native-libs`),
          extractedNativeLibsDirs = Seq(
            (`grpc-wrapper` / extractedFilesDir).value,
            (`conscrypt-wrapper` / extractedFilesDir).value
          ),
          extraJars = Seq(
            (`grpc-wrapper` / thinJarOutput).value,
            (`conscrypt-wrapper` / thinJarOutput).value
          )
        )
      stdGoogleJar
    },
    clean := Def.task {
      val _ = clean.value
      IO.delete(`google-polyglot-root`)
      IO.delete(`google-native-libs`)
    }.value
  )
  .dependsOn(`std-base` % "provided")
  .dependsOn(`std-table` % "provided")

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
      "org.graalvm.polyglot" % "polyglot"    % graalMavenPackagesVersion % "provided",
      "org.xerial"           % "sqlite-jdbc" % sqliteVersion,
      "org.postgresql"       % "postgresql"  % postgresVersion
    ),
    // Extract native libraries from sqlite-jdbc-**.jar and put them under
    // Standard/Database/polyglot/lib directory. The minimized jar will be
    // put under Standard/Database/polyglot/java directory.
    Compile / packageBin := {
      val logger            = streams.value.log
      val cacheStoreFactory = streams.value.cacheStoreFactory
      val stdDatabaseJar    = (Compile / packageBin).value
      StdBits
        .copyDependencies(
          `database-polyglot-root`,
          Seq("std-database.jar"),
          ignoreScalaLibrary = true,
          ignoreDependencyIncludeTransitive =
            Some(s"sqlite-jdbc-${sqliteVersion}"),
          libraryUpdates     = (Compile / update).value,
          logger             = streams.value.log,
          cacheStoreFactory  = cacheStoreFactory,
          unmanagedClasspath = (Compile / unmanagedJars).value,
          polyglotLibDir     = Some(`database-native-libs`),
          extractedNativeLibsDirs = Seq(
            (`sqlite-wrapper` / extractedFilesDir).value
          ),
          extraJars = Seq(
            (`sqlite-wrapper` / thinJarOutput).value
          )
        )
      stdDatabaseJar
    },
    clean := Def.task {
      val _ = clean.value
      IO.delete(`database-polyglot-root`)
      IO.delete(`database-native-libs`)
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
      "com.amazon.redshift"    % "redshift-jdbc42"       % redshiftVersion,
      "com.amazonaws"          % "aws-java-sdk-core"     % awsJavaSdkV1Version,
      "com.amazonaws"          % "aws-java-sdk-redshift" % awsJavaSdkV1Version,
      "com.amazonaws"          % "aws-java-sdk-sts"      % awsJavaSdkV1Version,
      "software.amazon.awssdk" % "auth"                  % awsJavaSdkV2Version,
      "software.amazon.awssdk" % "bom"                   % awsJavaSdkV2Version,
      "software.amazon.awssdk" % "s3"                    % awsJavaSdkV2Version,
      "software.amazon.awssdk" % "sso"                   % awsJavaSdkV2Version,
      "software.amazon.awssdk" % "ssooidc"               % awsJavaSdkV2Version
    ),
    Compile / packageBin := {
      val stdAwsJar         = (Compile / packageBin).value
      val cacheStoreFactory = streams.value.cacheStoreFactory
      StdBits
        .copyDependencies(
          `std-aws-polyglot-root`,
          Seq("std-aws.jar"),
          ignoreScalaLibrary = true,
          libraryUpdates     = (Compile / update).value,
          unmanagedClasspath = (Compile / unmanagedClasspath).value,
          logger             = streams.value.log,
          cacheStoreFactory
        )
      stdAwsJar
    },
    clean := Def.task {
      val _ = clean.value
      IO.delete(`std-aws-polyglot-root`)
    }.value
  )
  .dependsOn(`std-base` % "provided")
  .dependsOn(`std-database` % "provided")
  .dependsOn(`std-table` % "provided")

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
      "net.snowflake" % "snowflake-jdbc-thin" % snowflakeJDBCVersion exclude ("io.grpc", "grpc-xds")
    ),
    Compile / packageBin := {
      val logger            = streams.value.log
      val cacheStoreFactory = streams.value.cacheStoreFactory
      val stdSnowflakeJar   = (Compile / packageBin).value
      StdBits
        .copyDependencies(
          `std-snowflake-polyglot-root`,
          Seq("std-snowflake.jar"),
          ignoreScalaLibrary                = true,
          ignoreDependencyIncludeTransitive = Some(s"grpc-netty-shaded-1.60.0"),
          ignoreDependenciesByModuleID = Some(
            Seq(
              "org.conscrypt" % "conscrypt-openjdk-uber" % "2.5.2"
            )
          ),
          libraryUpdates     = (Compile / update).value,
          logger             = streams.value.log,
          cacheStoreFactory  = cacheStoreFactory,
          unmanagedClasspath = (Compile / unmanagedJars).value,
          polyglotLibDir     = Some(`std-snowflake-native-libs`),
          extractedNativeLibsDirs = Seq(
            (`grpc-wrapper-older` / extractedFilesDir).value,
            (`conscrypt-wrapper` / extractedFilesDir).value
          ),
          extraJars = Seq(
            (`grpc-wrapper-older` / thinJarOutput).value,
            (`conscrypt-wrapper` / thinJarOutput).value
          )
        )
      stdSnowflakeJar
    },
    clean := Def.task {
      val _ = clean.value
      IO.delete(`std-snowflake-polyglot-root`)
      IO.delete(`std-snowflake-native-libs`)
    }.value
  )
  .dependsOn(`std-base` % "provided")
  .dependsOn(`std-database` % "provided")
  .dependsOn(`std-table` % "provided")

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
      "com.microsoft.sqlserver"   % "mssql-jdbc"            % mssqlserverJDBCVersion,
      "com.azure"                 % "azure-identity"        % azureIdentityVersion exclude ("net.java.dev.jna", "jna") exclude ("net.java.dev.jna", "jna-platform"),
      "com.azure.resourcemanager" % "azure-resourcemanager" % azureResourceVersion,
      "com.azure"                 % "azure-storage-blob"    % azureBlobStorageVersion
    ),
    Compile / packageBin := {
      val logger            = streams.value.log
      val cacheStoreFactory = streams.value.cacheStoreFactory
      val stdMicrosoftJar   = (Compile / packageBin).value
      StdBits
        .copyDependencies(
          `std-microsoft-polyglot-root`,
          Seq("std-microsoft.jar"),
          ignoreScalaLibrary = true,
          libraryUpdates     = (Compile / update).value,
          unmanagedClasspath = (Compile / unmanagedClasspath).value,
          ignoreDependencies = Some((fileName: String) => {
            val nameCheck = fileName.startsWith(
              "netty-transport-native"
            ) || fileName.startsWith("netty-resolver-dns-native")

            (fileName.startsWith("netty-resolver-dns-classes-macos") && StdBits
              .plainOsName() != "macos") ||
            (fileName.startsWith("netty-tcnative-boringssl-static")) ||
            (fileName.startsWith("netty-transport-native-epoll")) ||
            nameCheck &&
            StdBits
              .allSupportedOs()
              .exists(osName => fileName.contains(osName)) && {
              val sanitizedName = fileName.replaceAll("aarch_64", "aarch64")
              val thisPlatform  = StdBits.currentPlatformSuffix()
              !sanitizedName.contains(thisPlatform)
            }
          }),
          logger         = logger,
          polyglotLibDir = Some(`std-microsoft-native-libs`),
          extractedNativeLibsDirs = Seq(
            (`jna-wrapper-extracted` / extractedFilesDir).value,
            (`netty-tc-native-wrapper` / extractedFilesDir).value,
            (`netty-resolver-dns-native-macos-wrapper` / extractedFilesDir).value
          ),
          // `netty-tc-native-wrapper / thinJarOutput` is not here on purpose.
          // It is an almost empty jar anyway.
          // The same is true for `netty-resolver-dns-native-macos-wrapper / thinJarOutput`.
          extraJars = Seq(
            (`jna-wrapper-extracted` / thinJarOutput).value
          ),
          cacheStoreFactory = cacheStoreFactory
        )
      stdMicrosoftJar
    },
    clean := Def.task {
      val _ = clean.value
      IO.delete(`std-microsoft-polyglot-root`)
      IO.delete(`std-microsoft-native-libs`)
    }.value
  )
  .dependsOn(`jna-wrapper` % "provided") // `azure-identity` requires `jna`
  .dependsOn(`std-base` % "provided")
  .dependsOn(`std-database` % "provided")
  .dependsOn(`std-table` % "provided")

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
    Compile / unmanagedClasspath :=
      (Compile / unmanagedClasspath)
        .dependsOn(fetchZipToUnmanaged)
        .value,
    Compile / unmanagedJars := (Compile / unmanagedJars)
      .dependsOn(fetchZipToUnmanaged)
      .value,
    Compile / packageBin / artifactPath :=
      `std-tableau-polyglot-root` / "std-tableau.jar",
    libraryDependencies ++= Seq(
    ),
    Compile / packageBin := {
      val logger             = streams.value.log
      val cacheStoreFactory  = streams.value.cacheStoreFactory
      val libraryUpdates     = (Compile / update).value
      val unmanagedClasspath = (Compile / unmanagedJars).value
      val stdTableauJar      = (Compile / packageBin).value
      StdBits
        .copyDependencies(
          `std-tableau-polyglot-root`,
          Seq("std-tableau.jar"),
          ignoreScalaLibrary = true,
          ignoreUnmanagedDependency =
            Some(!_.getName.endsWith("tableauhyperapi.jar")),
          libraryUpdates     = libraryUpdates,
          logger             = logger,
          cacheStoreFactory  = cacheStoreFactory,
          unmanagedClasspath = unmanagedClasspath,
          polyglotLibDir     = Some(`std-tableau-native-libs`),
          extractedNativeLibsDirs = Seq(
            (`jna-wrapper-extracted` / extractedFilesDir).value,
            (`tableau-wrapper` / extractedFilesDir).value
          ),
          extraJars = Seq(
            (`jna-wrapper-extracted` / thinJarOutput).value
          )
        )
      stdTableauJar
    },
    clean := Def.task {
      val _ = clean.value
      IO.delete(`std-tableau-polyglot-root`)
      IO.delete(`std-tableau-native-libs`)
    }.value
  )
  .dependsOn(`std-base` % "provided")
  .dependsOn(`std-table` % "provided")

lazy val `std-saas` = project
  .in(file("std-bits") / "saas")
  .settings(
    frgaalJavaCompilerSetting,
    autoScalaLibrary := false,
    Compile / compile / compileInputs := (Compile / compile / compileInputs)
      .dependsOn(SPIHelpers.ensureSPIConsistency)
      .value,
    Compile / packageBin / artifactPath :=
      `std-saas-polyglot-root` / "std-saas.jar",
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-email" % commonsEmailVersion
    ),
    Compile / packageBin := {
      val result            = (Compile / packageBin).value
      val cacheStoreFactory = streams.value.cacheStoreFactory
      StdBits
        .copyDependencies(
          `std-saas-polyglot-root`,
          Seq("std-saas.jar"),
          ignoreScalaLibrary = true,
          libraryUpdates     = (Compile / update).value,
          unmanagedClasspath = (Compile / unmanagedClasspath).value,
          logger             = streams.value.log,
          cacheStoreFactory
        )
      result
    }
  )
  .dependsOn(`std-base` % "provided")
  .dependsOn(`std-table` % "provided")

lazy val `std-duckdb` = project
  .in(file("std-bits") / "duckdb")
  .settings(
    frgaalJavaCompilerSetting,
    autoScalaLibrary := false,
    Compile / compile / compileInputs := (Compile / compile / compileInputs)
      .dependsOn(SPIHelpers.ensureSPIConsistency)
      .value,
    Compile / packageBin / artifactPath :=
      `std-duckdb-polyglot-root` / "std-duckdb.jar",
    libraryDependencies ++= Seq(
      "org.duckdb" % "duckdb_jdbc" % duckdbVersion % "provided"
    ),
    Compile / packageBin := {
      val stdDuckDBJar      = (Compile / packageBin).value
      val cacheStoreFactory = streams.value.cacheStoreFactory
      StdBits
        .copyDependencies(
          `std-duckdb-polyglot-root`,
          Seq("std-duckdb.jar"),
          ignoreScalaLibrary = true,
          libraryUpdates     = (Compile / update).value,
          unmanagedClasspath = (Compile / unmanagedClasspath).value,
          polyglotLibDir     = Some(`std-duckdb-native-libs`),
          ignoreDependencies = None,
          extractedNativeLibsDirs = Seq(
            (`duckdb-wrapper` / extractedFilesDir).value
          ),
          extraJars = Seq(
            (`duckdb-wrapper` / thinJarOutput).value
          ),
          logger            = streams.value.log,
          cacheStoreFactory = cacheStoreFactory
        )
      stdDuckDBJar
    },
    clean := Def.task {
      val _ = clean.value
      IO.delete(`std-duckdb-polyglot-root`)
      IO.delete(`std-duckdb-native-libs`)
    }.value
  )
  .dependsOn(`std-base` % "provided")
  .dependsOn(`std-database` % "provided")
  .dependsOn(`std-table` % "provided")

lazy val fetchZipToUnmanaged =
  taskKey[Seq[Attributed[File]]](
    "Download zip file from an `unmanagedExternalZip` url and unpack jars to unmanaged libs directory"
  )
lazy val unmanagedExternalZip =
  settingKey[URL]("URL to zip file with dependencies")

lazy val engineDistributionRoot =
  settingKey[File]("Root of built engine distribution")
lazy val launcherDistributionRoot =
  settingKey[File]("Root of built launcher distribution")

engineDistributionRoot :=
  packageBuilder.value.localArtifact("engine") / s"enso-$ensoVersion"
launcherDistributionRoot := packageBuilder.value.localArtifact(
  "launcher"
) / "enso"

lazy val extraBazelEnvForStdLibIndexes = taskKey[Map[String, String]](
  "Extra environment variables for subprocesses when running from Bazel - when compiling std libs"
)
extraBazelEnvForStdLibIndexes := Def.taskIf {
  if ((Bazel / wasStartedFromBazel).value) {
    val home     = (Bazel / homeDir).value.get.getAbsolutePath
    val repoRoot = (enso / baseDirectory).value
    val libPath =
      (engineDistributionRoot.value / "lib" / "Standard").getCanonicalPath
    val langHome = (engineDistributionRoot.value / "component").getCanonicalPath
    Map(
      "HOME"              -> home,
      "ENSO_HOME"         -> repoRoot.getAbsolutePath,
      "ENSO_EDITION_PATH" -> (repoRoot / "distribution" / "editions").getCanonicalPath,
      "JAVA_TOOL_OPTIONS" -> s"-Denso.languageHomeOverride=$langHome"
    )
  } else {
    val langHome = (engineDistributionRoot.value / "component").getCanonicalPath
    Map(
      "JAVA_TOOL_OPTIONS" -> s"-Denso.languageHomeOverride=$langHome"
    )
  }
}.value

lazy val createStdLibsIndexes =
  taskKey[Unit]("Creates index files for standard libraries")
createStdLibsIndexes := {
  buildEngineDistributionNoIndex.value
  val distributionRoot = engineDistributionRoot.value
  val log              = streams.value.log
  val cacheFactory     = streams.value.cacheStoreFactory
  val javaOpts         = (`engine-runner` / Runtime / javaOptions).value

  DistributionPackage.indexStdLibs(
    stdLibVersion = targetStdlibVersion,
    ensoVersion   = ensoVersion,
    libRoot       = distributionRoot / "lib",
    javaOpts      = javaOpts,
    env           = extraBazelEnvForStdLibIndexes.value,
    cacheFactory  = cacheFactory.sub("stdlib"),
    log           = log
  )
  log.info(s"Standard library indexes create for $distributionRoot")
}

ThisBuild / createStdLibsIndexes := {
  createStdLibsIndexes.result.value
}

def listRecursively(
  dir: File
): Seq[File] = {
  import scala.jdk.CollectionConverters.asScalaBufferConverter
  Files
    .walk(dir.toPath)
    .toList
    .asScala
    .map(_.toFile)
    .filter(_.isFile)
}

lazy val pythonHome = settingKey[File](
  "Output directory for extracted GraalPy resources."
)

ThisBuild / pythonHome := {
  val engineDir = engineDistributionRoot.value
  engineDir / "python-home"
}

lazy val createEnginePackageNoIndex =
  taskKey[Unit]("Creates the engine distribution package")
createEnginePackageNoIndex := {
  val modulesToCopy = componentModulesPaths.value
  val extraJars     = (`jline-wrapper` / thinJarOutput).value
  val nativeLibsDir = (`jline-wrapper` / extractedFilesDir).value
  val nativeLibs    = listRecursively(nativeLibsDir)
  val allFilesToCopy = modulesToCopy ++
    Seq(extraJars) ++
    nativeLibs

  val root            = engineDistributionRoot.value
  val pythonResources = (`python-extract` / extractPythonResources).value
  val pyHome          = (ThisBuild / pythonHome).value
  val log             = streams.value.log
  val cacheFactory    = streams.value.cacheStoreFactory
  DistributionPackage.createEnginePackage(
    distributionRoot    = root,
    cacheFactory        = cacheFactory,
    log                 = log,
    jarModulesToCopy    = allFilesToCopy,
    pythonResources     = pythonResources,
    pythonHome          = pyHome,
    graalVersion        = graalMavenPackagesVersion,
    javaVersion         = graalVersion,
    ensoVersion         = ensoVersion,
    editionName         = currentEdition,
    sourceStdlibVersion = stdLibVersion,
    targetStdlibVersion = targetStdlibVersion,
    targetDir           = (`syntax-rust-definition` / rustParserTargetDirectory).value
  )
  log.info(s"Engine package created at $root")
}

ThisBuild / createEnginePackageNoIndex := {
  createEnginePackageNoIndex.result.value
}

lazy val buildEngineDistributionNoIndex =
  taskKey[Unit](
    "Builds the engine distribution without generating indexes and optionally generating native image"
  )
buildEngineDistributionNoIndex := Def.taskIf {
  createEnginePackageNoIndex.value
  if (shouldBuildNativeImage.value) {
    (`ydoc-server` / buildNativeImage).value
    (`engine-runner` / buildNativeImage).value
    (`engine-runner` / checkNativeImageSize).value
  }
}.value

// This makes the buildEngineDistribution task usable as a dependency
// of other tasks.
ThisBuild / buildEngineDistributionNoIndex := {
  createEnginePackageNoIndex.value
}

lazy val shouldBuildNativeImage = taskKey[Boolean](
  "Whether native image should be build within buildEngineDistribution task"
)

ThisBuild / shouldBuildNativeImage := {
  GraalVM.EnsoLauncher.native
}

ThisBuild / NativeImage.additionalOpts := {
  if (!GraalVM.EnsoLauncher.native) {
    Seq()
  } else {
    var opts = if (GraalVM.EnsoLauncher.release) {
      // Picking `-Os` option instead of `-O3` in production on purpose.
      // See https://github.com/enso-org/enso/pull/12855#issuecomment-2812552448
      Seq("-Os")
    } else {
      Seq("-Ob")
    }

    if (GraalVM.EnsoLauncher.debug) {
      opts = opts ++ Seq("-H:GenerateDebugInfo=1")
    }
    if (GraalVM.EnsoLauncher.test) {
      opts = opts ++ Seq("-ea")
    }
    opts
  }
}

ThisBuild / engineDistributionRoot := {
  engineDistributionRoot.value
}

lazy val buildEngineDistribution =
  taskKey[Unit]("Builds the engine distribution")
buildEngineDistribution := {
  buildEngineDistributionNoIndex.value
  createEnginePackageNoIndex.value
  createStdLibsIndexes.value
}

// This makes the buildEngineDistributionNoIndex task usable as a dependency
// of other tasks.
ThisBuild / buildEngineDistribution := {
  buildEngineDistribution.result.value
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

lazy val lintEnso =
  inputKey[Unit](
    "Run Enso linter on one or many projects. If no arguments are specified, all projects are linted. Otherwise, the argument should be the full path or just the name of the project to lint."
  )
lintEnso := {
  buildEngineDistributionNoIndex.value
  val fileTree = fileTreeView.value

  val args: Seq[String] = spaceDelimited("<arg>").parsed
  val whatToLint = args match {
    case Seq()     => EnsoLint.LintTarget.All
    case Seq(name) => EnsoLint.LintTarget.FindByName(name)
    case _ =>
      throw new IllegalArgumentException(
        "At most one argument to lintEnso expected."
      )
  }

  val linter = new EnsoLint(
    baseDirectory.value,
    engineDistributionRoot.value,
    streams.value.log
  )
  linter.check(whatToLint)
}

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
