package org.enso.build.stdlibupdater

import org.enso.cli.OS

import java.nio.file.Path
import scala.sys.process._

/** Contains helper methods for running `prettier`. */
object Prettier {

  /** Formats a specific file or directory. */
  def format(path: Path): Unit = {
    val command =
      Seq(
        npxCommand,
        "prettier",
        "--write",
        path.toAbsolutePath.normalize.toString
      )
    val exitCode = command.!
    if (exitCode != 0) {
      throw new RuntimeException(s"$command failed with $exitCode exit code.")
    }
  }

  /** The platform-specific command that is used to run the `npx` tool. */
  def npxCommand: String = if (OS.isWindows) "npx.cmd" else "npx"
}
