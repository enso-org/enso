package org.enso.launcher

import org.scalatest.Suite
import org.scalatest.matchers.{MatchResult, Matcher}

import scala.sys.process._

trait NativeTest extends Suite {
  case class RunResult(exitCode: Int, stdout: String, stderr: String)

  object RunSuccessMatcher extends Matcher[RunResult] {
    override def apply(left: RunResult): MatchResult =
      MatchResult(
        left.exitCode == 0,
        s"Run did not exit with success but exit with code ${left.exitCode}.",
        s"Run did not fail as expected."
      )
  }

  def returnSuccess = RunSuccessMatcher

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
