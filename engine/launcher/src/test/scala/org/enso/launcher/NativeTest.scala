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
    * Runs the native launcher binary with the specified arguments and captures
    * its output.
    *
    * @param args arguments to forward to the launcher
    * @param extraEnv additional environment variables that should be overriden
    *                 for the launcher binary
    * @return
    */
  def runLauncher(
    args: Seq[String],
    extraEnv: (String, String)*
  ): RunResult = {
    val stdout = new StringBuilder
    val stderr = new StringBuilder
    val logger = new ProcessLogger {
      override def out(s: => String): Unit = stdout.append(s + "\n")

      override def err(s: => String): Unit = stderr.append(s + "\n")

      override def buffer[T](f: => T): T = f
    }

    try {
      val command  = Seq("./enso") ++ args
      val exitCode = Process(command, None, extraEnv: _*).!(logger)
      RunResult(exitCode, stdout.toString(), stderr.toString())
    } catch {
      case e: Exception =>
        throw new RuntimeException("Cannot run the Native Image binary", e)
    }
  }
}
