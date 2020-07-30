package org.enso.launcher

import java.nio.file.{Files, Path}

import org.scalatest.concurrent.{Signaler, TimeLimitedTests}
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.time.Span
import org.scalatest.wordspec.AnyWordSpec

import scala.sys.process._
import org.scalatest.time.SpanSugar._

import org.enso.launcher.internal.OS

/**
  * Contains helper methods for creating tests that need to run the native
  * launcher binary.
  */
trait NativeTest extends AnyWordSpec with Matchers with TimeLimitedTests {

  override val timeLimit: Span               = 15 seconds
  override val defaultTestSignaler: Signaler = _.interrupt()

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

  /**
    * Runs the native launcher binary with the specified arguments and captures
    * its output.
    *
    * @param args arguments to forward to the launcher
    * @param extraEnv environment variables to override for the launched
    *                 program, do not use these to override PATH
    */
  def runLauncher(
    args: Seq[String],
    extraEnv: Map[String, String] = Map.empty
  ): RunResult = {
    if (extraEnv.contains("PATH")) {
      throw new IllegalArgumentException(
        "To override path, use [[runLauncherWithPath]]."
      )
    }

    run(
      Seq(baseLauncherLocation.toAbsolutePath.toString) ++ args,
      extraEnv.toSeq
    )
  }

  /**
    * Runs the native launcher binary located at `pathToLauncher` with the
    * specified arguments and captures its output.
    *
    * @param pathToLauncher path to the launcher executable to run
    * @param args arguments to forward to the launcher
    * @param extraEnv environment variables to override for the launched
    *                 program, do not use these to override PATH
    */
  def runLauncherAt(
    pathToLauncher: Path,
    args: Seq[String],
    extraEnv: Map[String, String] = Map.empty
  ): RunResult = {
    if (extraEnv.contains("PATH")) {
      throw new IllegalArgumentException(
        "To override path, use [[runLauncherWithPath]]."
      )
    }

    run(Seq(pathToLauncher.toAbsolutePath.toString) ++ args, extraEnv.toSeq)
  }

  /**
    * Returns the expected location of the launcher binary compiled by the
    * Native Image. This binary can be copied into various places to test its
    * functionality.
    */
  def baseLauncherLocation: Path =
    Path.of(".").resolve(OS.executableName("enso"))

  /**
    * Creates a copy of the tested launcher binary at the specified location.
    */
  def copyLauncherTo(path: Path): Unit = {
    val parent = path.getParent
    Files.createDirectories(parent)
    Files.copy(baseLauncherLocation, path)
    if (!Files.isExecutable(path)) {
      throw new RuntimeException("Failed to make it executable...")
    }
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
    run(
      Seq(baseLauncherLocation.toAbsolutePath.toString) ++ args,
      Seq(pathName -> pathOverride)
    )
  }

  /**
    * This property can be temporarily set to true to allow for easier debugging
    * of native tests.
    */
  private val launcherDebugLogging = false

  private def run(
    command: Seq[String],
    extraEnv: Seq[(String, String)]
  ): RunResult = {
    val stdout = new StringBuilder
    val stderr = new StringBuilder
    val logger = new ProcessLogger {
      override def out(s: => String): Unit = {
        if (launcherDebugLogging) {
          System.err.println(s)
        }
        stdout.append(s + "\n")
      }

      override def err(s: => String): Unit = {
        if (launcherDebugLogging) {
          System.err.println(s)
        }
        stderr.append(s + "\n")
      }

      override def buffer[T](f: => T): T = f
    }

    try {
      val process = Process(command, None, extraEnv: _*).run(logger)
      try {
        RunResult(process.exitValue(), stdout.toString(), stderr.toString())
      } catch {
        case e: InterruptedException =>
          if (process.isAlive()) {
            println("Killing the timed-out process")
            process.destroy()
          }
          throw e
      }
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
