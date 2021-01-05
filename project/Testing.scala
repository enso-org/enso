import sbt.Def.spaceDelimited
import sbt.Keys._
import sbt._

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
        val OPTS_KEY = "JAVA_OPTS"
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
