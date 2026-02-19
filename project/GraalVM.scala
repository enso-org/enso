import sbt.Keys._
import sbt._
import sbt.internal.util.ManagedLogger
import sbt.io.IO
import sbt.librarymanagement.{ConfigurationFilter, DependencyFilter}

import scala.collection.immutable.Seq

/** A collection of utility methods for everything related to the GraalVM and Truffle.
  */
object GraalVM {
  object EnsoLauncher {
    val VAR_NAME = "ENSO_LAUNCHER"

    override def toString(): String = {
      val prop = System.getenv(VAR_NAME)
      // default value is `shell` for development and `native` for the release
      if (prop != null) {
        prop
      } else {
        if (BuildInfo.isReleaseMode) {
          "native"
        } else {
          "shell"
        }
      }
    }

    private lazy val parsed
      : (Boolean, Boolean, Boolean, Boolean, Boolean, Boolean) = {
      var shell                 = false
      var native                = false
      var test                  = false
      var debug                 = false
      var fast                  = false
      var disableLanguageServer = false
      toString().split(",").foreach {
        case "shell"  => shell  = true
        case "native" => native = true
        case "test" => {
          native = true
          test   = true
        }
        case "debug" => {
          native = true
          debug  = true
        }
        case "fast" => {
          native = true
          fast   = true
        }
        case "-ls" => {
          native                = true
          disableLanguageServer = true
        }
        case "-ms" => {
          native = true
        }
        case v =>
          throw new IllegalStateException(s"Unexpected value of $VAR_NAME: $v")
      }
      if (shell && native) {
        throw new IllegalStateException(
          s"Cannot specify `shell` and other properties in $VAR_NAME env variable"
        )
      }
      (
        shell,
        native,
        test,
        debug,
        fast,
        disableLanguageServer
      )
    }
    def native                = parsed._2
    def test                  = parsed._3
    def debug                 = parsed._4
    def fast                  = parsed._5
    def disableLanguageServer = parsed._6
    def release =
      native && !test && !debug && !fast && !disableLanguageServer
  }

  case class NativeImageSize(
    minMb: Int,
    maxMb: Int
  )

  object NativeImageSize {
    def expectedSizeForCurrentPlatform(): NativeImageSize = {
      if (EnsoLauncher.release) {
        if (Platform.isWindows) {
          windowsX64Release
        } else if (Platform.isLinux) {
          linuxX64Release
        } else if (Platform.isMacOS && Platform.isArm64) {
          macARM64Release
        } else {
          throw new IllegalArgumentException(
            s"Unexpected platform: ${Platform.arch()} ${Platform.osName()}"
          )
        }
      } else {
        testNISize
      }
    }

    // Expected production NI sizes deduced from sizes on latest
    // nightly builds: https://github.com/enso-org/enso/pull/14565#issue-3781936779
    // With maximal size relaxed by 30 MB.
    private val windowsX64Release = NativeImageSize(100, 273)
    private val linuxX64Release   = NativeImageSize(100, 300)
    private val macARM64Release   = NativeImageSize(100, 273)
    private val testNISize        = NativeImageSize(100, 350)
  }

  /** Has the user requested to use Espresso for Java interop? */
  private def isEspressoMode(): Boolean =
    "espresso".equals(System.getenv("ENSO_JAVA"))

  private val version: String = Dependencies.graalMavenPackagesVersion

  final def mavenPackagesVersion: String = version

  /** The list of modules that are included in the `component` directory in engine distribution.
    * When invoking the `java` command, these modules need to be put on the module-path.
    */
  val modules: Seq[ModuleID] = Seq(
    "org.graalvm.sdk"      % "nativeimage"      % version,
    "org.graalvm.sdk"      % "word"             % version,
    "org.graalvm.sdk"      % "jniutils"         % version,
    "org.graalvm.sdk"      % "collections"      % version,
    "org.graalvm.polyglot" % "polyglot"         % version,
    "org.graalvm.truffle"  % "truffle-api"      % version,
    "org.graalvm.truffle"  % "truffle-runtime"  % version,
    "org.graalvm.truffle"  % "truffle-compiler" % version
  )

  private val sdkPkgs = Seq(
    "org.graalvm.sdk" % "polyglot-tck" % version,
    "org.graalvm.sdk" % "nativeimage"  % version,
    "org.graalvm.sdk" % "word"         % version,
    "org.graalvm.sdk" % "jniutils"     % version,
    "org.graalvm.sdk" % "collections"  % version
  )

  private val polyglotPkgs = Seq(
    "org.graalvm.polyglot" % "polyglot" % version
  )

  private val trufflePkgs = Seq(
    "org.graalvm.truffle" % "truffle-api"           % version,
    "org.graalvm.truffle" % "truffle-runtime"       % version,
    "org.graalvm.truffle" % "truffle-compiler"      % version,
    "org.graalvm.truffle" % "truffle-dsl-processor" % version
  )

  /** Manually maintained GraalVM languages and their dependencies. Optimally,
    * we would use 'org.graalvm.polyglot:js-community' or 'org.graavm.polyglot:python-community'
    * maven artifacts and all their transitive dependencies, but we have to copy all these artifacts
    * into engine distribution build, so we have to maintain these manually.
    */

  private val pythonPkgs =
    Seq(
      "org.graalvm.python"   % "python-language"    % version,
      "org.graalvm.python"   % "python-resources"   % version,
      "org.bouncycastle"     % "bcutil-jdk18on"     % "1.78.1",
      "org.bouncycastle"     % "bcpkix-jdk18on"     % "1.78.1",
      "org.bouncycastle"     % "bcprov-jdk18on"     % "1.78.1",
      "org.graalvm.llvm"     % "llvm-api"           % version,
      "org.graalvm.truffle"  % "truffle-nfi"        % version,
      "org.graalvm.truffle"  % "truffle-nfi-panama" % version,
      "org.graalvm.truffle"  % "truffle-nfi-libffi" % version,
      "org.graalvm.regex"    % "regex"              % version,
      "org.graalvm.tools"    % "profiler-tool"      % version,
      "org.graalvm.shadowed" % "json"               % version,
      "org.graalvm.shadowed" % "icu4j"              % version,
      "org.graalvm.shadowed" % "xz"                 % version
    )

  val jsPkgs =
    Seq(
      "org.graalvm.js"       % "js-language" % version,
      "org.graalvm.regex"    % "regex"       % version,
      "org.graalvm.shadowed" % "icu4j"       % version
    )

  val chromeInspectorPkgs = Seq(
    "org.graalvm.tools"    % "chromeinspector-tool" % version,
    "org.graalvm.shadowed" % "json"                 % version,
    "org.graalvm.tools"    % "profiler-tool"        % version
  )

  private val debugAdapterProtocolPkgs = Seq(
    "org.graalvm.tools" % "dap-tool" % version
  )

  val insightPkgs = Seq(
    "org.graalvm.tools" % "insight-tool"      % version,
    "org.graalvm.tools" % "insight-heap-tool" % version
  )

  private val espressoPkgs =
    Seq(
      "org.graalvm.truffle"  % "truffle-nfi"                      % version,
      "org.graalvm.truffle"  % "truffle-nfi-libffi"               % version,
      "org.graalvm.espresso" % "espresso-language"                % version,
      "org.graalvm.espresso" % "espresso-libs-resources"          % version,
      "org.graalvm.espresso" % "espresso-runtime-resources-jdk21" % version
    )

  val toolsPkgs = chromeInspectorPkgs ++ debugAdapterProtocolPkgs ++ insightPkgs

  val langsPkgs =
    if (isEspressoMode()) {
      espressoPkgs
    } else {
      jsPkgs ++ pythonPkgs
    }

  private val allowedJavaVendors = Seq(
    "GraalVM Community",
    "Oracle Corporation"
  )

  private val downloadLink =
    s"https://github.com/graalvm/graalvm-ce-builds/releases/tag/jdk-${Dependencies.graalVersion}"

  /** Augments a state transition to do GraalVM version check.
    *
    * @param graalVersion  the GraalVM version that should be used for
    *                      building this project
    * @param graalPackagesVersion Version of Truffle and GraalVM packages that
    *                             will be downloaded from Maven
    * @param javaVersion Version of the Java source code
    * @return an augmented state transition that does all the state changes of
    *         oldTransition but also runs the version checks
    */
  def versionCheck(
    graalVersion: String,
    graalPackagesVersion: String,
    javaVersion: String,
    oldState: State
  ): State = {
    val log = oldState.log
    if (graalPackagesVersion != version) {
      log.error(
        s"Expected GraalVM packages version $version, but got $graalPackagesVersion. " +
        s"Version specified in build.sbt and GraalVM.scala must be in sync"
      )
      return oldState.fail
    }
    val javaVendor = System.getProperty("java.vendor")
    if (!allowedJavaVendors.contains(javaVendor)) {
      log.warn(
        s"Running on non-GraalVM JVM (The actual java.vendor is $javaVendor). " +
        s"Expected Java vendors: ${allowedJavaVendors.mkString(", ")}. " +
        s"Download link: $downloadLink"
      )
    }

    val javaSpecVersion = System.getProperty("java.specification.version")
    if (javaSpecVersion != javaVersion) {
      log.error(
        s"Running on Java version $javaSpecVersion. " +
        s"Expected Java version $javaVersion. " +
        s"Download link: $downloadLink"
      )
      return oldState.fail
    }

    val vmVersion = System.getProperty("java.vm.version")
    tryParseJavaVMVersion(vmVersion) match {
      case Some(version) =>
        if (!isSameVersion(version, graalVersion)) {
          log.error(
            s"Running on GraalVM version $version. " +
            s"Expected GraalVM version $graalVersion. " +
            s"Download link: $downloadLink"
          )
          oldState.fail
        } else {
          oldState
        }
      case None =>
        log.error(
          s"Could not parse GraalVM version from java.vm.version: $vmVersion."
        )
        oldState.fail
    }
  }

  private def tryParseJavaVMVersion(
    version: String
  ): Option[String] = {
    if (version.contains('+')) {
      Some(version.split('+')(0))
    } else {
      None
    }
  }

  private def isSameVersion(s1: String, s2: String): Boolean = {
    if (s1 == s2) {
      true
    } else {
      val semVer1 = toSemVer(s1)
      val semVer2 = toSemVer(s2)
      semVer1 == semVer2
    }
  }

  private def toSemVer(ver: String): SemVer = {
    try {
      if (ver.contains(".")) {
        val items = ver.split('.')
        if (items.length == 2) {
          SemVer(Integer.parseInt(items(0)), Integer.parseInt(items(1)), 0)
        } else if (items.length == 3) {
          SemVer(
            Integer.parseInt(items(0)),
            Integer.parseInt(items(1)),
            Integer.parseInt(items(2))
          )
        } else {
          throw new IllegalArgumentException(s"Cannot parse version: $ver")
        }
      } else {
        SemVer(Integer.parseInt(ver), 0, 0)
      }
    } catch {
      case e: NumberFormatException =>
        throw new IllegalArgumentException(s"Cannot parse version: $ver", e)
    }
  }

  private case class SemVer(major: Int, minor: Int, patch: Int)
}
