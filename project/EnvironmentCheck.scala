import sbt._
import sbt.internal.util.ManagedLogger

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
  def checkVersions(
    expectedGraalVersion: String,
    expectedJavaVersion: String,
    expectedFlatbuffersVersion: String,
    log: ManagedLogger
  ): Unit = {
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

    val versionsOk = graalOk && javaOk
    if (!versionsOk) {
      log.error(
        "=== Please make sure to change to a correct version of" +
        " GraalVM before attempting compilation ==="
      )
    }

    GenerateFlatbuffers.verifyFlatcVersion(expectedFlatbuffersVersion) match {
      case Left(explanation) =>
        log.error(explanation)
        log.error(
          "=== Please make sure to install a correct version of" +
          " flatc before attempting compilation ==="
        )
      case Right(_) =>
    }
  }

  /**
    * Augments a state transition to do a GraalVM version check.
    *
    * @param graalVersion the GraalVM version that should be used for
    *                     building this project
    * @param javaVersion the Java version of the used GraalVM distribution
    * @param flatbuffersVersion the Flatbuffers library version
    * @param oldTransition the state transition to be augmented
    * @return an augmented state transition that does all the state changes of
    *         oldTransition but also runs the GraalVM version check
    */
  def addVersionCheck(
    graalVersion: String,
    javaVersion: String,
    flatbuffersVersion: String
  )(
    oldTransition: State => State
  ): State => State =
    (state: State) => {
      val newState = oldTransition(state)
      checkVersions(graalVersion, javaVersion, flatbuffersVersion, newState.log)
      newState
    }
}
