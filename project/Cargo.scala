import sbt.Keys._
import sbt._
import sbt.internal.util.ManagedLogger

import scala.sys.process._

/** A wrapper for executing the command `cargo`. */
object Cargo {
  private val cargoCmd            = "cargo"
  private val rustUpCmd           = "rustup"
  private var wasCargoOk: Boolean = false

  /** Executes the command `cargo $args`. */
  def apply(args: Seq[String]): Def.Initialize[Task[Unit]] =
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
    args: Seq[String],
    log: ManagedLogger,
    extraEnv: Seq[(String, String)] = Seq()
  ): Unit = {
    val cmd: Seq[String] = Seq(cargoCmd) ++ args

    if (!cargoOk(log))
      throw new RuntimeException("Cargo isn't installed!")

    log.debug(cmd.toString())

    val process = Process(cmd, None, extraEnv: _*)
    val sb      = new StringBuilder
    val processLogger = ProcessLogger(str => {
      log.debug(str)
      sb.append(str)
        .append(System.lineSeparator())
    })
    val exitCode =
      try process.!(processLogger)
      catch {
        case e: RuntimeException =>
          log.error(s"`$cargoCmd` command failed to run.")
          log.error(sb.toString())
          throw e
      }
    if (exitCode != 0) {
      log.error(s"`$cargoCmd` command failed with exit code $exitCode.")
      log.error(sb.toString())
    }
  }

  def rustUp(target: String, log: ManagedLogger): Unit = {
    val cmd: Seq[String] = Seq(rustUpCmd) ++ Seq("target", "add", target)

    log.info(cmd.toString())

    val exitCode =
      try Process(cmd, None).!
      catch {
        case _: RuntimeException =>
          throw new RuntimeException(s"`$rustUpCmd` command failed to run.")
      }
    if (exitCode != 0) {
      throw new RuntimeException(
        s"`$rustUpCmd` command returned a non-zero exit code: $exitCode."
      )
    }

  }

  /** Checks that cargo is installed. Logs an error and returns false if not. */
  def cargoOk(log: ManagedLogger): Boolean = if (wasCargoOk) true
  else {
    try {
      s"$cargoCmd version".!!
      wasCargoOk = true
      true
    } catch {
      case _: RuntimeException =>
        log.error(s"The command `cargo` isn't on path. Did you install cargo?")
        false
    }
  }
}
