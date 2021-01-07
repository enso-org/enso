import sbt.Def.spaceDelimited
import sbt.Keys._
import sbt._
import java.io.File

object Testing {
  val registeredSuites: Map[String, String] = Map(
    "base"  -> "Tests",
    "table" -> "Table_Tests"
  )

  /** Command allowing to run a test suite.
    *
    * Usage:
    * testEnso <suite-name> [<JVM options>]
    */
  def testEnso(distributionRoot: File) = Def.inputTask {
    val log               = streams.value.log
    val runnerBinary      = if (Platform.isWindows) "enso.bat" else "enso"
    val runnerPath        = (distributionRoot / "bin" / runnerBinary).absolutePath
    val args: Seq[String] = spaceDelimited("<arg>").parsed
    args match {
      case Seq(suiteName, jvmOpts @ _*) =>
        val suiteDirectory = registeredSuites.getOrElse(
          suiteName, {
            val suites = registeredSuites.keys.mkString(", ")
            log.error(s"Need a test suite name. Possible values are $suites.")
            throw new IllegalArgumentException("Invalid suite name.")
          }
        )
        val suitePath = (file("test") / suiteDirectory).absolutePath

        val command = Seq(runnerPath, "--run", suitePath)
        log.info(s"Running ${command.mkString(" ")}")
        val pb       = new ProcessBuilder(command: _*)
        val PATH_KEY = if (Platform.isWindows) "Path" else "PATH"
        val OPTS_KEY = "JAVA_OPTS"

        val jvmPath = ProcessHandle.current().info().command().asScala
        jvmPath match {
          case Some(str) =>
            val binPath = file(str).getParentFile.absolutePath
            val path = {
              val path = pb.environment().getOrDefault(PATH_KEY, "")
              if (path.isEmpty) binPath else binPath + File.pathSeparator + path
            }
            pb.environment().put(PATH_KEY, path)
            log.info(s"Will use the JVM at $binPath")
          case None =>
            log.warn(
              "Could not determine path of the currently running JVM, " +
              "the test suite will be run on the default configured JVM " +
              "which may not work correctly if it is not the right JVM."
            )
        }

        val opts =
          pb.environment().getOrDefault(OPTS_KEY, "") + " " + jvmOpts
            .mkString(" ")
        pb.environment().put(OPTS_KEY, opts)

        pb.inheritIO()
        val process  = pb.start()
        val exitCode = process.waitFor()
        if (exitCode != 0) {
          log.error(s"Test suite failed with exit code $exitCode")
          throw new RuntimeException("Test suite failed.")
        }
      case _ =>
        val suites = registeredSuites.keys.mkString(", ")
        log.error(s"Need a test suite name. Possible values are $suites.")
        throw new IllegalArgumentException("Missing suite name.")
    }
  }
}
