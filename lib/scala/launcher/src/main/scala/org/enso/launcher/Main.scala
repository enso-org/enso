package org.enso.launcher

import cats.implicits._
import com.monovore.decline._

object MainCommandBuilder {
  private val JSON_OPTION = "json"

  private def newCommand: Command[Unit] =
    Command("new", "Create a new Enso project.") {
      val nameOpt =
        Opts.option[String]("name", help = "Name of the created project.")
      val pathOpt =
        Opts
          .option[String](
            "path",
            help = "Path specifying where to create the project."
          )
          .orNone

      (nameOpt, pathOpt).mapN { (name, path) =>
        Launcher.newProject(name, path)
      }
    }

  private def versionCommand: Command[Unit] =
    Command(
      "version",
      "Print version of the launcher and currently selected Enso distribution."
    ) {
      val jsonFlag = Opts
        .flag(
          JSON_OPTION,
          "Whether to print the version as JSON instead of plain text."
        )
        .orFalse
      jsonFlag.map(Launcher.version)
    }

  def buildCommands: Opts[Unit] = {
    Opts.subcommands(
      versionCommand,
      newCommand
    ) // TODO this is a work in progress
  }

  buildCommands
}

object Main
    extends CommandApp(
      name   = "enso",
      header = "Enso launcher.",
      main   = MainCommandBuilder.buildCommands
    )
