import java.io.IOException

import sbt._
import sbt.internal.util.ManagedLogger

import scala.sys.process._

object EnvironmentCheck {

  /** Compares the version of JVM running sbt with the GraalVM versions defined
    * in project configuration and reports errors if the versions do not match.
    *
    * @param expectedGraalVersion the GraalVM version that should be used for
    *                             building this project
    * @param expectedJavaVersion the Java version of the used GraalVM
    *                            distribution
    * @param log a logger used to report errors if the versions are mismatched
    */
  def graalVersionOk(
    expectedGraalVersion: String,
    expectedJavaVersion: String,
    log: ManagedLogger
  ): Boolean = {
    val javaSpecificationVersion =
      System.getProperty("java.vm.specification.version")
    val graalVersion =
      System.getProperty("org.graalvm.version")

    val graalOk =
      if (graalVersion == null) {
        log.error(
          "Property org.graalvm.version is not defined. " +
          s"Make sure your current JVM is set to " +
          s"GraalVM $expectedGraalVersion Java $expectedJavaVersion"
        )
        false
      } else if (graalVersion != expectedGraalVersion) {
        log.error(
          s"GraalVM version mismatch - you are running $graalVersion but " +
          s"$expectedGraalVersion is expected"
        )
        false
      } else true

    val javaOk =
      if (javaSpecificationVersion != expectedJavaVersion) {
        log.error(
          s"Java version mismatch - you are running " +
          s"Java $javaSpecificationVersion " +
          s"but Java $expectedJavaVersion is expected"
        )
        false
      } else true

    graalOk && javaOk
  }

  /**
    * Runs `rustc --version` to ensure that it is properly installed and
    * checks if the reported version is consistent with expectations.
    *
    * @param expectedVersion rust version that is expected to be installed,
    *                        should be based on project settings
    * @return either an error message explaining what is wrong with the rust
    *         version or Unit meaning it is correct
    */
  def rustVersionOk(expectedVersion: String, log: ManagedLogger): Boolean = {
    val cmd = "rustc --version"

    try {
      val versionStr = cmd.!!.trim.substring(6)

      if (versionStr != expectedVersion) log.error(
        s"Rust version mismatch. $expectedVersion is expected, " +
          s"but it seems $versionStr is installed."
      )
      versionStr == expectedVersion
    } catch {
      case _ @(_: RuntimeException | _: IOException) =>
        log.error("Rust version check failed. Make sure rustc is in your PATH.")
        false
    }
  }

  /**
    * Augments a state transition to do a Rust and GraalVM version check.
    *
    * @param graalVersion the GraalVM version that should be used for
    *                     building this project
    * @param javaVersion the Java version of the used GraalVM distribution
    * @param oldTransition the state transition to be augmented
    * @return an augmented state transition that does all the state changes of
    *         oldTransition but also runs the version checks
    */
  def addVersionCheck(
    graalVersion: String,
    javaVersion: String
  )(
    oldTransition: State => State
  ): State => State =
    (state: State) => {
      val newState = oldTransition(state)
      val logger   = newState.log
      val graalOk  = graalVersionOk(graalVersion, javaVersion, logger)
      if (!graalOk)
        throw new RuntimeException("Some versions checks failed.")

      newState
    }
}
