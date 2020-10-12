package org.enso.launcher

import java.nio.file.Path

import cats.implicits._
import com.monovore.decline._

object MainCommandBuilder {
  private val JSON_OPTION = "json"

  private def newCommand: Command[Unit] =
    Command("new", "Create a new Enso project.") {
      val nameOpt = Opts.argument[String]("name")
      val pathOpt = Opts.argument[Path]("path").orNone

      (nameOpt, pathOpt).mapN { (name, path) =>
        Launcher.newProject(name, path)
      }
    }

  private val jsonFlag = Opts
    .flag(
      JSON_OPTION,
      "Whether to print the version as JSON instead of plain text."
    )
    .orFalse

  private def versionCommand: Command[Unit] =
    Command(
      "version",
      "Print version of the launcher and currently selected Enso distribution."
    ) {
      jsonFlag.map(Launcher.displayVersion)
    }

  private def versionOption: Opts[Unit] = {
    val versionTrigger = Opts
      .flag(
        "version",
        "Print version of the launcher and currently selected Enso distribution."
      )
    (versionTrigger, jsonFlag).mapN { (_, useJSON) =>
      Launcher.displayVersion(useJSON)
    }
  }

  def buildCommands: Opts[Unit] = {
    versionOption orElse
    Opts.subcommands(
      versionCommand,
      newCommand
    )
  }
}

object Main
    extends CommandApp(
      name   = "enso",
      header = "Enso launcher.",
      main   = MainCommandBuilder.buildCommands
    )
