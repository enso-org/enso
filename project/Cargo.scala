import sbt.Keys._
import sbt._
import sbt.internal.util.ManagedLogger

import scala.sys.process._

/** A wrapper for executing the command `cargo`. */
object Cargo {
  private val cargoCmd = "cargo"

  /** Executes the command `cargo $args`. */
  def apply(args: String): Def.Initialize[Task[Unit]] =
    Def.task {
      run(args, state.value.log)
    }

  /** Executes the command `cargo $args`.
    *
    * @param args arguments to pass to cargo
    * @param log a logger instance for diagnostics
    * @param extraEnv additional environment variables that should be set for
    *                 the cargo process
    */
  def run(
    args: String,
    log: ManagedLogger,
    extraEnv: Seq[(String, String)] = Seq()
  ): Unit = {
    val cmd = s"$cargoCmd $args"

    if (!cargoOk(log))
      throw new RuntimeException("Cargo isn't installed!")

    log.info(cmd)

    val exitCode =
      try Process(cmd, None, extraEnv: _*).!
      catch {
        case _: RuntimeException =>
          throw new RuntimeException("Cargo command failed to run.")
      }
    if (exitCode != 0) {
      throw new RuntimeException(
        s"Cargo command returned a non-zero exit code: $exitCode."
      )
    }
  }

  /** Checks that cargo is installed. Logs an error and returns false if not. */
  def cargoOk(log: ManagedLogger): Boolean = {
    try s"$cargoCmd version".!!
    catch {
      case _: RuntimeException =>
        log.error(s"The command `cargo` isn't on path. Did you install cargo?")
        return false
    }
    true
  }
}
