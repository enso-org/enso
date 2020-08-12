import sbt.Keys._
import sbt._
import sbt.internal.util.ManagedLogger

import scala.sys.process._

object Cargo {

  val rustVersion = settingKey[String]("rustc version used in the project")
  private val cargoCmd = "cargo"

  def build(args: String): Def.Initialize[Task[Unit]] = Def.task {
    val log = state.value.log
    val cmd = s"$cargoCmd build $args"

    log.info(cmd)

    try cmd.!! catch {
      case ex: RuntimeException =>
        log.error(s"Cargo build failed.")
        throw ex
    }
  }
}
