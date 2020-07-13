package org.enso.launcher

import java.nio.file.Path
import java.util.UUID

import org.enso.launcher.cli.{
  Argument,
  Command,
  Commands,
  Opts,
  PluginBehaviour,
  PluginInterceptedFlow,
  PluginNotFound
}
import org.enso.launcher.cli.Opts._
import cats.implicits._

object Main {
  private def jsonFlag: Opts[Boolean] = Opts.flag("json")

  private def versionCommand: Command =
    Command(
      "version",
      "Print version of the launcher and currently selected Enso distribution."
    ) {
      jsonFlag map Launcher.displayVersion
    }

  private def newCommand: Command =
    Command("new", "Create a new Enso project.") {
      val nameOpt = Opts.positionalArgument[String]("name")
      val pathOpt = Opts.optionalArgument[Path]("path")

      (nameOpt, pathOpt) mapN { (name, path) =>
        Launcher.newProject(name, path)
      }
    }

  private def runCommand: Command =
    Command("run", "TODO") {
      val pathOpt        = Opts.optionalArgument[Path]("path")
      val jvmArgs        = Opts.prefixedParameters("jvm")
      val additionalArgs = Opts.additionalParameters
      (pathOpt, jvmArgs, additionalArgs) mapN {
        (path, jvmArgs, additionalArgs) =>
          println(s"Launch runner for $path")
          println(s"JVM=$jvmArgs, additionalArgs=$additionalArgs")
      }
    }

  private def languageServerCommand: Command =
    Command("language-server", "TODO") {
      val rootId         = Opts.parameter[UUID]("root-id")
      val path           = Opts.parameter[Path]("path")
      val jvmArgs        = Opts.prefixedParameters("jvm")
      val additionalArgs = Opts.additionalParameters
      (rootId, path, jvmArgs, additionalArgs) mapN {
        (rootId, path, jvmArgs, additionalArgs) =>
          println(s"Launch language server in $path with id=$rootId.")
          println(s"JVM=$jvmArgs, additionalArgs=$additionalArgs")
      }
    }

  private def replCommand: Command =
    Command("repl", "TODO") {
      val path           = Opts.optionalParameter[Path]("path")
      val jvmArgs        = Opts.prefixedParameters("jvm")
      val additionalArgs = Opts.additionalParameters
      (path, jvmArgs, additionalArgs) mapN { (path, jvmArgs, additionalArgs) =>
        println(s"Launch REPL in $path")
        println(s"JVM=$jvmArgs, additionalArgs=$additionalArgs")
      }
    }

  private def defaultCommand: Command =
    Command("default", "TODO") {
      val version = Opts.optionalArgument[String]()
      version map {
        case Some(version) => println(s"Set version to $version")
        case None          => println("Print current version")
      }
    }

  private def upgradeCommand: Command =
    Command("upgrade", "TODO") {
      val version = Opts.optionalArgument[String]()
      version map {
        case Some(version) => println(s"Upgrade launcher to $version")
        case None          => println("Upgrade launcher to latest")
      }
    }

  private def installCommand: Command =
    Command("install", "TODO") {
      val version = Opts.positionalArgument[String]()
      version map { version =>
        println(s"Install $version")
      }
    }

  private def uninstallCommand: Command =
    Command("uninstall", "TODO") {
      val version = Opts.positionalArgument[String]()
      version map { version =>
        println(s"Uninstall $version")
      }
    }

  private def listCommand: Command =
    Command("list", "TODO") {
      sealed trait Components
      case object EnsoComponents    extends Components
      case object RuntimeComponents extends Components
      implicit val argumentComponent: Argument[Components] = {
        case "enso"    => EnsoComponents.asRight
        case "runtime" => RuntimeComponents.asRight
        case other =>
          (s"Unknown argument '$other' - expected 'enso', 'runtime' " +
          "or no argument to print a general summary.").asLeft
      }

      val what = Opts.optionalArgument[Components]()
      what map {
        case Some(EnsoComponents)    => println("List enso")
        case Some(RuntimeComponents) => println("List runtime")
        case None                    => println("List summary")
      }
    }

  private def configCommand: Command =
    Command("config", "TODO") {
      val global = Opts.flag("global")
      val key    = Opts.positionalArgument[String]("key")
      val value  = Opts.positionalArgument[String]("value")
      (key, value, global) mapN { (key, value, global) =>
        val which = if (global) "global" else "local"
        println(s"Set in the $which config $key => $value")
      }
    }

  private def helpCommand: Command =
    Command("help", "TODO") {
      pure(()) map { _ => printTopLevelHelp() }
    }

  private def specialOptions: Opts[Unit] = {
    val help    = Opts.flag("help", 'h')
    val version = Opts.flag("version", 'V')
    val json    = jsonFlag
    (help, version, json) mapN {
      case (help, version, useJSON) =>
        if (help || !version) {
          printTopLevelHelp()
        } else {
          Launcher.displayVersion(useJSON)
        }
    }
  }

  private def findPlugin(name: String): Boolean = name == "ide" // TODO [RW]

  private def pluginHandler(name: String, args: Seq[String]): PluginBehaviour =
    findPlugin(name) match {
      case true =>
        println(s"Would launch $name with $args") // TODO [RW] launch plugin
        PluginInterceptedFlow
      case false =>
        PluginNotFound
    }

  private def commandsConfig: Commands =
    Commands(
      Seq(
        versionCommand,
        helpCommand,
        newCommand,
        replCommand,
        runCommand,
        languageServerCommand,
        defaultCommand,
        installCommand,
        uninstallCommand,
        upgradeCommand,
        listCommand,
        configCommand
      ),
      pluginHandler
    )

  private def printTopLevelHelp(): Unit = {
    println("TODO help")
  }

  def main(args: Array[String]): Unit = {
    val arguments = args.toSeq
    Opts.parse(specialOptions)(arguments) match {
      case Right(()) =>
      case Left(_) =>
        Commands.parse(commandsConfig)(arguments) match {
          case Left(errors) =>
            println(errors.mkString("\n"))
            System.exit(1)
          case Right(()) =>
        }
    }
  }
}
