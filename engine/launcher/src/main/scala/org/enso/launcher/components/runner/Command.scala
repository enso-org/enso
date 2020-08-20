package org.enso.launcher.components.runner

import org.enso.launcher.Logger

import scala.sys.process.{Process, ProcessBuilder}
import scala.util.{Failure, Try}

/**
  * Represents information required to run a system command.
  *
  * @param command the command and its arguments that should be executed
  * @param extraEnv environment variables that should be overridden
  */
case class Command(command: Seq[String], extraEnv: Seq[(String, String)]) {

  /**
    * Runs the command and returns its exit code.
    *
    * May return an exception if it is impossible to run the command (for
    * example due to insufficient permissions or nonexistent executable).
    */
  def run(): Try[Int] =
    runProcess { processBuilder =>
      processBuilder.run(connectInput = true).exitValue()
    }

  /**
    * Runs the command and returns its standard output as [[String]].
    *
    * The standard error is printed to the console.
    *
    * May return an exception if it is impossible to run the command or the
    * command returned non-zero exit code.
    */
  def captureOutput(): Try[String] =
    runProcess { processBuilder =>
      processBuilder.!!
    }

  /**
    * Prepares to run a process and invokes `action` on the created
    * [[ProcessBuilder]], capturing any errors.
    *
    * On success, the result of `action` is returned.
    */
  private def runProcess[R](action: ProcessBuilder => R): Try[R] = {
    val result = Try {
      Logger.debug(s"Executing $toString")
      val processBuilder = Process(command, None, extraEnv: _*)
      action(processBuilder)
    }
    result.recoverWith(error =>
      Failure(
        RunnerError(
          s"Could not run the command $toString due to: $error",
          error
        )
      )
    )
  }

  /**
    * A textual representation of the command in a format that can be copied in
    * to a terminal and executed.
    */
  override def toString: String = {
    def escapeQuotes(string: String): String =
      "\"" + string.replace("\"", "\\\"") + "\""
    val environmentDescription =
      extraEnv.map(v => s"${v._1}=${escapeQuotes(v._2)} ").mkString
    val commandDescription = command.map(escapeQuotes).mkString(" ")
    environmentDescription + commandDescription
  }
}
