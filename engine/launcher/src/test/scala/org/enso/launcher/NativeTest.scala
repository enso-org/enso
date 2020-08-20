package org.enso.launcher

import java.nio.file.{Files, Path}
import java.lang.{ProcessBuilder => JProcessBuilder}

import org.scalatest.concurrent.{Signaler, TimeLimitedTests}
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.time.Span
import org.scalatest.wordspec.AnyWordSpec

import org.scalatest.time.SpanSugar._

import scala.collection.Factory
import scala.jdk.CollectionConverters._
import scala.jdk.StreamConverters._

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
        s"Run did not exit with success but exit with code ${left.exitCode}.\n" +
        s"Its stderr was: ```${left.stderr}```.\n" +
        s"And stdout was: ```${left.stdout}```.",
        s"Run did not fail as expected. It printed ```${left.stdout}```."
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
    *
    * It waits a 100ms delay after creating the copy to ensure that the copy can
    * be called right away after calling this function. It is not absolutely
    * certain that this is helpful, but from time to time, the tests fail
    * because the filesystem does not allow to access the executable as
    * 'not-ready'. This delay is an attempt to make the tests more stable.
    */
  def copyLauncherTo(path: Path): Unit = {
    val parent = path.getParent
    Files.createDirectories(parent)
    Files.copy(baseLauncherLocation, path)
    if (!Files.isExecutable(path)) {
      throw new RuntimeException("Failed to make it executable...")
    }
    Thread.sleep(100)
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
    * Runs the provided `command`.
    *
    * `extraEnv` may be provided to extend the environment. Care must be taken
    * on Windows where environment variables are (mostly) case-insensitive.
    *
    * If `waitForDescendants` is set, tries to wait for descendants of the
    * launched process to finish too. Especially important on Windows where
    * child processes may run after the launcher parent has been terminated.
    */
  private def run(
    command: Seq[String],
    extraEnv: Seq[(String, String)],
    waitForDescendants: Boolean = true
  ): RunResult = {
    val builder = new JProcessBuilder(command: _*)
    val newKeys = extraEnv.map(_._1.toLowerCase)
    if (newKeys.distinct.size < newKeys.size) {
      throw new IllegalArgumentException(
        "The extra environment keys have to be unique"
      )
    }

    lazy val existingKeys =
      builder.environment().keySet().asScala
    for ((key, value) <- extraEnv) {
      if (OS.isWindows) {
        def shadows(key1: String, key2: String): Boolean =
          key1.toLowerCase == key2.toLowerCase && key1 != key2

        existingKeys.find(shadows(_, key)) match {
          case Some(oldKey) =>
            throw new IllegalArgumentException(
              s"The environment key `$key` may be shadowed by `$oldKey` " +
              s"already existing in the environment. Please use `$oldKey`."
            )
          case None =>
        }
      }
      builder.environment().put(key, value)
    }

    try {
      val process = builder.start()

      try {
        val exitCode = process.waitFor()
        if (waitForDescendants) {
          val descendants = process.descendants().toScala(Factory.arrayFactory)
          descendants.foreach(_.onExit().join())
        }
        val stdout = new String(process.getInputStream.readAllBytes())
        val stderr = new String(process.getErrorStream.readAllBytes())
        RunResult(exitCode, stdout, stderr)
      } catch {
        case e: InterruptedException =>
          if (process.isAlive) {
            println(s"Killing the timed-out process: ${command.mkString(" ")}")
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
