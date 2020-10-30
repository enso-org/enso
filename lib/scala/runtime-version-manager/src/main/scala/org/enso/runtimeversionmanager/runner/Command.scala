package org.enso.runtimeversionmanager.runner

import com.typesafe.scalalogging.Logger

import scala.sys.process.Process
import scala.util.{Failure, Try}

/** Represents information required to run a system command.
  *
  * @param command the command and its arguments that should be executed
  * @param extraEnv environment variables that should be overridden
  */
case class Command(command: Seq[String], extraEnv: Seq[(String, String)]) {
  private val logger = Logger[Command]

  /** Runs the command and returns its exit code.
    *
    * May return an exception if it is impossible to run the command (for
    * example due to insufficient permissions or nonexistent executable).
    */
  def run(): Try[Int] =
    wrapError {
      logger.debug(s"Executing $toString")
      val processBuilder = new java.lang.ProcessBuilder(command: _*)
      for ((key, value) <- extraEnv) {
        processBuilder.environment().put(key, value)
      }
      processBuilder.inheritIO()
      val process = processBuilder.start()
      process.waitFor()
    }

  /** Runs the command and returns its standard output as [[String]].
    *
    * The standard error is printed to the console.
    *
    * May return an exception if it is impossible to run the command or the
    * command returned non-zero exit code.
    */
  def captureOutput(): Try[String] =
    wrapError {
      logger.debug(s"Executing $toString")
      val processBuilder = Process(command, None, extraEnv: _*)
      processBuilder.!!
    }

  /** Runs the provided action and wraps any errors into a [[Failure]]
    * containing a [[RunnerError]].
    */
  private def wrapError[R](action: => R): Try[R] =
    Try(action).recoverWith(error =>
      Failure(
        RunnerError(
          s"Could not run the command $toString due to: $error",
          error
        )
      )
    )

  /** A textual representation of the command in a format that can be copied in
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
