package org.enso.launcher

import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.wordspec.AnyWordSpec

import scala.sys.process._

/**
  * Contains helper methods for creating tests that need to run the native
  * launcher binary.
  */
trait NativeTest extends AnyWordSpec with Matchers {

  /**
    * A result of running the native launcher binary.
    *
    * @param exitCode the returned exit code
    * @param stdout contents of the standard output stream
    * @param stderr contents of the standard error stream
    */
  case class RunResult(exitCode: Int, stdout: String, stderr: String)

  object RunSuccessMatcher extends Matcher[RunResult] {
    override def apply(left: RunResult): MatchResult =
      MatchResult(
        left.exitCode == 0,
        s"Run did not exit with success but exit with code ${left.exitCode}.",
        s"Run did not fail as expected."
      )
  }

  /**
    * A scalatest Matcher that allows to write expressions like
    * `run should returnSuccess`.
    */
  def returnSuccess: RunSuccessMatcher.type = RunSuccessMatcher

  /**
    * Specifies if the current OS is Windows.
    */
  val isWindows: Boolean =
    System.getProperty("os.name").toLowerCase().contains("windows")

  private val launcherCommand = "./enso"

  /**
    * Runs the native launcher binary with the specified arguments and captures
    * its output.
    *
    * @param args arguments to forward to the launcher
    */
  def runLauncher(
    args: Seq[String]
  ): RunResult = {
    run(Seq(launcherCommand) ++ args, Seq())
  }

  /**
    * Runs the native launcher binary with the specified arguments and captures
    * its output.
    *
    * @param args arguments to forward to the launcher
    * @param pathOverride the system PATH that should be set for the launched
    *                     program
    */
  def runLauncherWithPath(
    args: Seq[String],
    pathOverride: String
  ): RunResult = {
    val pathName = if (isWindows) "Path" else "PATH" // Note [Windows Path]
    run(Seq(launcherCommand) ++ args, Seq(pathName -> pathOverride))
  }

  private def run(
    command: Seq[String],
    extraEnv: Seq[(String, String)]
  ): RunResult = {
    val stdout = new StringBuilder
    val stderr = new StringBuilder
    val logger = new ProcessLogger {
      override def out(s: => String): Unit = stdout.append(s + "\n")

      override def err(s: => String): Unit = stderr.append(s + "\n")

      override def buffer[T](f: => T): T = f
    }

    try {
      val exitCode = Process(command, None, extraEnv: _*).!(logger)
      RunResult(exitCode, stdout.toString(), stderr.toString())
    } catch {
      case e: Exception =>
        throw new RuntimeException("Cannot run the Native Image binary", e)
    }
  }
}

/* Note [Windows Path]
 * ~~~~~~~~~~~~~~~~~~~
 * On Windows, the environment variables are usually treated as case-insensitive
 * but not all the time. When launching a process with an added environment
 * variable called `PATH`, that process actually has two variables in its
 * environment - the original `Path` and the overriden `PATH`. This can be seen
 * when querying `System.getenv()` - the returned map contains both `Path` and
 * `PATH` with their respective values. However, `System.getenv("PATH")` uses
 * some platform specific logic, and even if both variables are present in the
 * environment, it actually returns the value corresponding to `Path`. This is
 * likely the expected behaviour on Windows. So to successfully override the
 * system path on Windows, we need to override `Path`, not `PATH` like on
 * Unix-based systems.
 */
