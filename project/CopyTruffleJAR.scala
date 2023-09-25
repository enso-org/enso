import sbt.Keys._
import sbt._
import sbt.internal.util.ManagedLogger

object CopyTruffleJAR {

  /** The task that is used for copying the Truffle JAR file to our target
    * directory. It should be ran when setting-up the project and after each
    * update of Graal version.
    *
    * As the JARs have to be available to the sbt JVM on startup, to ensure the
    * JARs are available, the whole sbt process must be restarted (otherwise the
    * compilation would fail with cryptic errors). To make sure that the user
    * restarts the process, it is terminated.
    */
  lazy val bootstrapJARs = Def.task {
    val log = streams.value.log
    if (
      ensureTruffleJARUpToDate(
        baseDirectory.value,
        (Compile / update).value,
        log
      )
    ) {
      log.info("Truffle JARs have been updated.")
      System.err.println(
        "You have to restart the sbt JVM for the changes to take effect."
      )
      System.out.flush()
      System.err.flush()
      System.exit(0)
    } else {
      log.info("Truffle JARs are up to date.")
    }
  }

  /** This task should be added as a dependency of compileInputs in the runtime
    * subproject. It ensures that the compilation will not proceed unless the
    * JARs have not been bootstrapped. If the JARs were out of date, they are
    * updated within this task, so bootstrap does not have to be re-run.
    * However, for the same reasons as for [[bootstrapJARs]], the sbt process is
    * terminated, because a restart is required.
    */
  lazy val preCompileTask = Def.task {
    val log = streams.value.log
    if (
      ensureTruffleJARUpToDate(
        baseDirectory.value,
        (Compile / update).value,
        log
      )
    ) {
      log.error(
        "JARs that have to be loaded by the sbt JVM at startup have been" +
        " modified or did not exist.\n" +
        "Did you run bootstrap?"
      )
      log.warn(
        "Bootstrap has been triggered automatically, but the sbt JVM must be" +
        " restarted to apply the changes, so the compilation had to be stopped."
      )
      log.warn(
        "To avoid disrupting compilation, remember to run bootstrap when " +
        "setting up the project and after each version change of Graal."
      )
      System.err.println(
        "The sbt JVM has to be restarted to apply the changes.\n" +
        "Please re-launch sbt and re-run the last command."
      )
      System.out.flush()
      System.err.flush()
      System.exit(0)
    }
  }

  /** Checks the Truffle JARs and updates them if necessary.
    *
    * @param libraryUpdates the value of Compile / updates
    * @return true if an update has been performed and the JVM needs a restart
    */
  private def ensureTruffleJARUpToDate(
    baseDirectory: File,
    libraryUpdates: UpdateReport,
    log: ManagedLogger
  ): Boolean = {
    val version = System.getProperty("java.version").split("\\.");
    if (Integer.parseInt(version(0)) >= 21) {
      // skip all checks on GraalVM for JDK21 and newer
      return false;
    }
    var truffleInstancesFound = 0
    var restartRequired       = false
    libraryUpdates.allFiles.foreach { f =>
      if (f.getName.contains("truffle-api")) {
        truffleInstancesFound += 1
        val dest = baseDirectory / "build-cache" / "truffle-api.jar"
        val needsUpdate = if (!dest.exists()) {
          log.debug("truffle-api.jar does not exist in target/")
          true
        } else if (dest.lastModified() < f.lastModified()) {
          log.debug("truffle-api.jar in target/ is out of date")
          true
        } else false

        if (needsUpdate) {
          IO.copyFile(f, dest)
          restartRequired = true
        }
      }
    }

    if (truffleInstancesFound == 0) {
      throw new IllegalStateException(
        "Truffle API has not been found in the dependencies!\n" +
        "If dependencies have been changed in build.sbt, make sure " +
        "the algorithm locating truffle-api.jar in" +
        " project/CopyTruffleJAR.scala has been updated accordingly.\n" +
        "Please report this as a bug."
      )
    } else if (truffleInstancesFound > 1) {
      throw new IllegalStateException(
        "More than one version of Truffle API has been found in the " +
        "dependencies.\n" +
        "If dependencies have been changed in build.sbt, make sure " +
        "the algorithm locating truffle-api.jar in" +
        " project/CopyTruffleJAR.scala has been updated accordingly.\n" +
        "Please report this as a bug."
      )
    }

    restartRequired
  }
}
