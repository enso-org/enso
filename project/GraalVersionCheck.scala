import java.io.IOException
import sbt.*
import sbt.internal.util.ManagedLogger

import scala.sys.process.*
import nl.gn0s1s.bump.SemVer

object GraalVersionCheck {

  /** Compares the version of JVM running sbt with the GraalVM versions defined
    * in project configuration and reports errors if the versions do not match.
    *
    * @param expectedGraalVersionRaw the GraalVM version that should be used for
    *                             building this project
    * @param log a logger used to report errors if the versions are mismatched
    */
  def graalVersionOk(
    expectedGraalVersionRaw: String,
    log: ManagedLogger
  ): Boolean = {
    val expectedGraalVersion = SemVer(expectedGraalVersionRaw)
    require(expectedGraalVersion.isDefined, "Invalid version string")

    val versionProperty = "java.vendor.version"
    val rawGraalVersion = System.getProperty(versionProperty)

    def graalVersion: Option[SemVer] = {
      val versionRegex = """GraalVM (CE|EE) ([\d.]+.*)""".r
      rawGraalVersion match {
        case versionRegex(_, version) =>
          SemVer(version)
        case _ => None
      }
    }

    if (rawGraalVersion == null) {
      log.error(
        s"Property $versionProperty is not defined. " +
        s"Make sure your current JVM is set to " +
        s"GraalVM $expectedGraalVersionRaw."
      )
      false
    } else {
      graalVersion match {
        case Some(version)
            if expectedGraalVersion.get.withoutBuildMetadata == version.withoutBuildMetadata =>
          true
        case _ =>
          log.error(
            s"GraalVM version mismatch - you are running $rawGraalVersion " +
            s"but GraalVM $expectedGraalVersionRaw is expected."
          )
          false
      }
    }
  }

  /** Augments a state transition to do a Rust and GraalVM version check.
    *
    * @param graalVersion the GraalVM version that should be used for
    *                     building this project
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
      if (!graalVersionOk(graalVersion, logger)) {
        logger.error("GraalVM version check failed.")
        System.exit(1)
      }
      newState
    }
}
