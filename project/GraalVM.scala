import sbt.Keys._
import sbt._
import sbt.internal.util.ManagedLogger
import sbt.io.IO
import sbt.librarymanagement.{ConfigurationFilter, DependencyFilter}

import scala.collection.immutable.Seq

/** A collection of utility methods for everything related to the GraalVM and Truffle.
  */
object GraalVM {

  /** Has the user requested to use Espresso for Java interop? */
  private def isEspressoMode(): Boolean =
    "espresso".equals(System.getenv("ENSO_JAVA"))

  // Keep in sync with graalMavenPackagesVersion in build.sbt
  private val version: String = "24.0.0"

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
      "org.bouncycastle"     % "bcutil-jdk18on"     % "1.76",
      "org.bouncycastle"     % "bcpkix-jdk18on"     % "1.76",
      "org.bouncycastle"     % "bcprov-jdk18on"     % "1.76",
      "org.graalvm.llvm"     % "llvm-api"           % version,
      "org.graalvm.truffle"  % "truffle-nfi"        % version,
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
      "org.graalvm.truffle"  % "truffle-nfi"                            % version,
      "org.graalvm.truffle"  % "truffle-nfi-libffi"                     % version,
      "org.graalvm.espresso" % "espresso-language"                      % version,
      "org.graalvm.espresso" % "espresso-libs-resources-linux-amd64"    % version,
      "org.graalvm.espresso" % "espresso-runtime-resources-linux-amd64" % version
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
        s"Expected Java vendors: ${allowedJavaVendors.mkString(", ")}."
      )
    }

    val javaSpecVersion = System.getProperty("java.specification.version")
    if (javaSpecVersion != javaVersion) {
      log.error(
        s"Running on Java version $javaSpecVersion. " +
        s"Expected Java version $javaVersion."
      )
      return oldState.fail
    }

    val vmVersion = System.getProperty("java.vm.version")
    tryParseJavaVMVersion(vmVersion) match {
      case Some(version) =>
        if (version != graalVersion) {
          log.error(
            s"Running on GraalVM version $version. " +
            s"Expected GraalVM version $graalVersion."
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
}
