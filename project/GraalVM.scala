import sbt.Keys.*
import sbt.*
import sbt.internal.util.ManagedLogger
import sbt.io.IO
import sbt.librarymanagement.{ConfigurationFilter, DependencyFilter}

import scala.collection.immutable.Seq

/** A collection of utility methods for everything related to the GraalVM and Truffle.
  */
object GraalVM {
  // Keep in sync with graalMavenPackagesVersion in build.sbt
  val version: String = "23.1.0"

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

  val sdkPkgs = Seq(
    "org.graalvm.sdk" % "polyglot-tck" % version,
    "org.graalvm.sdk" % "nativeimage"  % version,
    "org.graalvm.sdk" % "word"         % version,
    "org.graalvm.sdk" % "jniutils"     % version,
    "org.graalvm.sdk" % "collections"  % version
  )

  val polyglotPkgs = Seq(
    "org.graalvm.polyglot" % "polyglot" % version
  )

  val trufflePkgs = Seq(
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

  val pythonPkgs = Seq(
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
    "org.tukaani"          % "xz"                 % "1.9"
  )

  val jsPkgs = Seq(
    "org.graalvm.js"       % "js-language" % version,
    "org.graalvm.regex"    % "regex"       % version,
    "org.graalvm.shadowed" % "icu4j"       % version
  )

  val chromeInspectorPkgs = Seq(
    "org.graalvm.tools"    % "chromeinspector-tool" % version,
    "org.graalvm.shadowed" % "json"                 % version,
    "org.graalvm.tools"    % "profiler-tool"        % version
  )

  val debugAdapterProtocolPkgs = Seq(
    "org.graalvm.tools" % "dap-tool" % version
  )

  val insightPkgs = Seq(
    "org.graalvm.tools" % "insight-tool" % version
  )

  val espressoPkgs = if ("espresso".equals(System.getenv("ENSO_JAVA"))) {
    Seq(
      "org.graalvm.espresso" % "espresso-language"                      % version,
      "org.graalvm.espresso" % "espresso-libs-resources-linux-amd64"    % version,
      "org.graalvm.espresso" % "espresso-runtime-resources-linux-amd64" % version
    )
  } else {
    Seq()
  }

  val toolsPkgs = chromeInspectorPkgs ++ debugAdapterProtocolPkgs ++ insightPkgs

  val langsPkgs = jsPkgs ++ pythonPkgs ++ espressoPkgs

  /** Augments a state transition to do GraalVM version check.
    *
    * @param graalVersion  the GraalVM version that should be used for
    *                      building this project
    * @param oldTransition the state transition to be augmented
    * @return an augmented state transition that does all the state changes of
    *         oldTransition but also runs the version checks
    */
  def addVersionCheck(
    graalVersion: String
  )(
    oldTransition: State => State
  ): State => State =
    (state: State) => {
      val newState = oldTransition(state)
      val logger   = newState.log
      if (graalVersion != version) {
        logger.error("GraalVM version check failed.")
        throw new IllegalStateException(
          s"Expected GraalVM version $version, but got $graalVersion. " +
          s"Version specified in build.sbt and GraalVM.scala must be in sync"
        )
      }
      newState
    }
}
