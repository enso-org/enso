import sbt.Keys._
import sbt._
import sbt.internal.util.ManagedLogger

import scala.sys.process._

object Cargo {

  val rustVersion = settingKey[String]("rustc version used in the project")
  private val cargoCmd = "cargo"

  def apply(args: String): Def.Initialize[Task[Unit]] = Def.task {
    val log = state.value.log
    val cmd = s"$cargoCmd $args"

    if (!cargoOk(log))
      throw new RuntimeException("Cargo isn't installed!")

    if (!EnvironmentCheck.rustVersionOk(rustVersion.value, log))
      throw new RuntimeException("Rust version mismatch!")

    log.info(cmd)

    try cmd.!! catch {
      case ex: RuntimeException =>
        log.error(s"Cargo build failed.")
        throw ex
    }
  }

  def cargoOk(log: ManagedLogger): Boolean = {
    try s"$cargoCmd version".!! catch {
      case _: RuntimeException =>
        log.error(s"The command `cargo` isn't on path. Have you cargo installed?")
        return false
    }
    true
  }
}
