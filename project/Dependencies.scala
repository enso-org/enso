import sbt._
import org.enso.build.BenchTasks.Benchmark

/** Dependencies of the whole project and their versions.
  *
  * Note [Dependency Versions]
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
  *
  * Note [Engine And Launcher Version]
  * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  * Currently both Engine and Launcher versions are tied to each other - each new
  * releases contains the Engine and the Launcher and thus the version number is
  * shared. If the version numbers ever diverge, make sure to update the build
  * scripts at .github/workflows accordingly.
  *
  * Note [Default Editions]
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
  *
  * Note [Stdlib Version]
  * ~~~~~~~~~~~~~~~~~~~~~
  * The `stdlibVersion` variable stores the version at which standard library is
  * stored within the source tree, which is currently set to a constant of
  * `0.0.0-dev`.
  *
  * When distributions are built, the library versions are updated to match the
  * current Enso version.
  */
object Dependencies {
  // === project-wide versions =====================================================
  val scalacVersion = "2.13.15"
  // source version of the Java language
  val javaVersion = "25"
  // version of the GraalVM JDK
  val graalVersion = "25.0.1"
  // Version used for the Graal/Truffle related Maven packages
  // Keep in sync with GraalVM.version. Do not change the name of this variable,
  // it is used by the Rust build script via regex matching.
  val graalMavenPackagesVersion = "25.0.1"
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

  // === Akka ===================================================================

  def akkaPkg(name: String)     = akkaURL %% s"akka-$name" % akkaVersion
  def akkaHTTPPkg(name: String) = akkaURL %% s"akka-$name" % akkaHTTPVersion
  val akkaURL                   = "com.typesafe.akka"
  val akkaVersion               = "2.6.20"
  val akkaHTTPVersion           = "10.2.10"
  val akkaMockSchedulerVersion  = "0.5.5"
  val reactiveStreamsVersion    = "1.0.3"
  val sprayJsonVersion          = "1.3.6"
  val logbackClassicVersion     = "1.3.7"
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
  val commonsEmailVersion       = "1.5"
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
  val helidonVersion = "4.2.2"
  val helidon = {
    val clientAndSharedDeps = Seq(
      "io.helidon"               % "helidon"                     % helidonVersion,
      "io.helidon.builder"       % "helidon-builder-api"         % helidonVersion,
      "io.helidon.common"        % "helidon-common"              % helidonVersion,
      "io.helidon.common"        % "helidon-common-buffers"      % helidonVersion,
      "io.helidon.common"        % "helidon-common-config"       % helidonVersion,
      "io.helidon.common"        % "helidon-common-configurable" % helidonVersion,
      "io.helidon.common"        % "helidon-common-context"      % helidonVersion,
      "io.helidon.common"        % "helidon-common-key-util"     % helidonVersion,
      "io.helidon.common"        % "helidon-common-mapper"       % helidonVersion,
      "io.helidon.common"        % "helidon-common-media-type"   % helidonVersion,
      "io.helidon.common"        % "helidon-common-parameters"   % helidonVersion,
      "io.helidon.common"        % "helidon-common-resumable"    % helidonVersion,
      "io.helidon.common"        % "helidon-common-socket"       % helidonVersion,
      "io.helidon.common"        % "helidon-common-tls"          % helidonVersion,
      "io.helidon.common"        % "helidon-common-types"        % helidonVersion,
      "io.helidon.common"        % "helidon-common-uri"          % helidonVersion,
      "io.helidon.http"          % "helidon-http"                % helidonVersion,
      "io.helidon.http.encoding" % "helidon-http-encoding"       % helidonVersion,
      "io.helidon.http.media"    % "helidon-http-media"          % helidonVersion,
      "io.helidon.logging"       % "helidon-logging-common"      % helidonVersion,
      "io.helidon.metadata"      % "helidon-metadata-hson"       % helidonVersion,
      "io.helidon.service"       % "helidon-service-metadata"    % helidonVersion,
      "io.helidon.service"       % "helidon-service-registry"    % helidonVersion,
      "io.helidon.webclient"     % "helidon-webclient"           % helidonVersion,
      "io.helidon.webclient"     % "helidon-webclient-api"       % helidonVersion,
      "io.helidon.webclient"     % "helidon-webclient-http1"     % helidonVersion,
      "io.helidon.webclient"     % "helidon-webclient-websocket" % helidonVersion,
      "io.helidon.websocket"     % "helidon-websocket"           % helidonVersion
    )
    val serverDeps = Seq(
      "io.helidon.webserver"          % "helidon-webserver"                 % helidonVersion,
      "io.helidon.webserver"          % "helidon-webserver-websocket"       % helidonVersion,
      "io.helidon.config"             % "helidon-config"                    % helidonVersion,
      "io.helidon.common"             % "helidon-common-security"           % helidonVersion,
      "io.helidon.common.concurrency" % "helidon-common-concurrency-limits" % helidonVersion,
      "io.helidon.common.features"    % "helidon-common-features"           % helidonVersion,
      "io.helidon.common.features"    % "helidon-common-features-api"       % helidonVersion,
      "io.helidon.common"             % "helidon-common-task"               % helidonVersion,
      "io.helidon.metrics"            % "helidon-metrics-api"               % helidonVersion
    )
    clientAndSharedDeps ++ serverDeps
  }

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

  // === Scala =========================================================
  val scalaReflect = Seq(
    "org.scala-lang" % "scala-reflect" % scalacVersion
  )
  val scalaLibrary = Seq(
    "org.scala-lang" % "scala-library" % scalacVersion
  )
  val scalaParserCombinatorsVersion = "1.1.2"
  val scalaJavaCompatVersion        = "1.0.0"
  val scalaCollectionCompatVersion  = "2.8.1"

  // === std-lib ================================================================

  // Has to match Truffle's ANTLR dependency version to avoid spurious warnings in Native Image
  val antlrVersion            = "4.12.0"
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

  // === Bouncy Castle ==========================================================

  val bouncyCastleVersion = "1.78.1"
  val bouncyCastle = Seq(
    "org.bouncycastle" % "bcutil-jdk18on" % bouncyCastleVersion,
    "org.bouncycastle" % "bcpkix-jdk18on" % bouncyCastleVersion,
    "org.bouncycastle" % "bcprov-jdk18on" % bouncyCastleVersion
  )

  // === JLine ==================================================================
  val jlineVersion = "3.26.3"
  val jlineNative = Seq(
    "org.jline" % "jline-native" % jlineVersion
  )
  val jline = Seq(
    "org.jline" % "jline-terminal"     % jlineVersion,
    "org.jline" % "jline-terminal-jni" % jlineVersion, // The terminal provider jna has been deprecated, check your configuration.
    "org.jline" % "jline-reader"       % jlineVersion
  ) ++ jlineNative

  // === Google =================================================================
  val googleApiClientVersion         = "2.7.1"
  val googleApiServicesSheetsVersion = "v4-rev20250106-2.0.0"
  val googleAnalyticsAdminVersion    = "0.66.0"
  val googleAnalyticsDataVersion     = "0.67.0"
  val grpcVersion                    = "1.69.0"

  // === SLF4J ==================================================================
  val slf4jVersion = "2.0.16"
  val slf4jApi = Seq(
    "org.slf4j" % "slf4j-api" % slf4jVersion
  )
  val slf4jNop       = "org.slf4j" % "slf4j-nop" % slf4jVersion
  val slf4jNopModule = "org.slf4j.nop"

  // === Other ==================================================================

  val declineVersion          = "2.4.1"
  val diffsonVersion          = "4.4.0"
  val directoryWatcherVersion = "0.18.0"
  val flatbuffersVersion      = "24.3.25"
  val guavaVersion            = "32.0.0-jre"
  val jgitVersion             = "6.7.0.202309050840-r"
  val kindProjectorVersion    = "0.13.3"
  val mockitoScalaVersion     = "1.17.14"
  val mockitoJavaVersion      = "5.20.0"
  val newtypeVersion          = "0.4.4"
  val pprintVersion           = "0.8.1"
  val pureconfigVersion       = "0.17.4"
  val scalacheckVersion       = "1.18.1"
  val scalacticVersion        = "3.2.19"
  val scalaLoggingVersion     = "3.9.4"
  val scalameterVersion       = "0.19"
  val scalatestVersion        = "3.2.19"
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
  val azureIdentityVersion    = "1.16.1"
  val azureResourceVersion    = "2.50.0"
  val azureBlobStorageVersion = "12.30.0"
  val jsoniterVersion         = "2.28.5"
  val jnaVersion              = "5.14.0"
  val googleProtobufVersion   = "3.25.1"
  val shapelessVersion        = "2.3.10"
  val postgresVersion         = "42.4.0"
  val duckdbVersion           = "1.4.0.0"
  val h2Version               = "2.3.232"
  val jimFsVersion            = "1.3.0"
}
