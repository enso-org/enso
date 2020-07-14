package org.enso.launcher

import java.nio.file.Path
import java.util.UUID

import org.enso.launcher.cli.{
  Application,
  Argument,
  Command,
  CommandHelp,
  Opts,
  PluginBehaviour,
  PluginInterceptedFlow,
  PluginManager,
  PluginNotFound
}
import org.enso.launcher.cli.Opts._
import cats.implicits._

object Main {
  private def jsonFlag: Opts[Boolean] =
    Opts.flag("json", "Use JSON instead of plain text for output.")

  private def versionCommand: Command =
    Command(
      "version",
      "Print version of the launcher and currently selected Enso distribution."
    ) {
      jsonFlag map { useJSON => () => Launcher.displayVersion(useJSON) }
    }

  private def newCommand: Command =
    Command("new", "Create a new Enso project.") {
      val nameOpt = Opts.positionalArgument[String]("name", "Project name.")
      val pathOpt = Opts.optionalArgument[Path](
        "path",
        "Path where to create the project. " +
        "By default a directory called <name> is created in the current directory."
      )

      (nameOpt, pathOpt) mapN { (name, path) => () =>
        Launcher.newProject(name, path)
      }
    }

  private def jvmArgs =
    Opts.prefixedParameters(
      "jvm",
      "Parameters prefixed with jvm (--jvm.key=value) will be passed to the launched JVM as -Dkey=value."
    )

  private def runCommand: Command =
    Command("run", "Run a project or Enso script.") {
      val pathOpt        = Opts.optionalArgument[Path]("path")
      val additionalArgs = Opts.additionalArguments()
      (pathOpt, jvmArgs, additionalArgs) mapN {
        (path, jvmArgs, additionalArgs) => () =>
          println(s"Launch runner for $path")
          println(s"JVM=$jvmArgs, additionalArgs=$additionalArgs")
      }
    }

  private def languageServerCommand: Command =
    Command(
      "language-server",
      "Launch the Language Server for a given project."
    ) {
      val rootId         = Opts.parameter[UUID]("root-id", "uuid", "TODO explain.")
      val path           = Opts.parameter[Path]("path", "path", "Project path.")
      val additionalArgs = Opts.additionalArguments()
      (rootId, path, jvmArgs, additionalArgs) mapN {
        (rootId, path, jvmArgs, additionalArgs) => () =>
          println(s"Launch language server in $path with id=$rootId.")
          println(s"JVM=$jvmArgs, additionalArgs=$additionalArgs")
      }
    }

  private def replCommand: Command =
    Command("repl", "Launch an Enso REPL.") {
      val path           = Opts.optionalParameter[Path]("path", "path", "Project path.")
      val additionalArgs = Opts.additionalArguments()
      (path, jvmArgs, additionalArgs) mapN {
        (path, jvmArgs, additionalArgs) => () =>
          println(s"Launch REPL in $path")
          println(s"JVM=$jvmArgs, additionalArgs=$additionalArgs")
      }
    }

  private def defaultCommand: Command =
    Command("default", "Print or change the default Enso version.") {
      val version = Opts.optionalArgument[String](
        "version",
        "If provided, sets default version to <version>. " +
        "Otherwise, current default is displayed."
      )
      version map { version => () =>
        version match {
          case Some(version) => println(s"Set version to $version")
          case None          => println("Print current version")
        }
      }
    }

  private def upgradeCommand: Command =
    Command("upgrade", "Upgrade the launcher.") {
      val version = Opts.optionalArgument[String](
        "version",
        "Version to upgrade to. " +
        "If not provided, defaults to latest version."
      )
      version map { version => () =>
        version match {
          case Some(version) => println(s"Upgrade launcher to $version")
          case None          => println("Upgrade launcher to latest version")
        }
      }
    }

  private def installCommand: Command =
    Command("install", "Installs a selected Enso version.") {
      val version = Opts.positionalArgument[String]("version")
      version map { version => () =>
        println(s"Install $version")
      }
    }

  private def uninstallCommand: Command =
    Command("uninstall", "Uninstall an Enso version.") {
      val version = Opts.positionalArgument[String]("version")
      version map { version => () =>
        println(s"Uninstall $version")
      }
    }

  private def listCommand: Command =
    Command("list", "List installed components.") {
      sealed trait Components
      case object EnsoComponents    extends Components
      case object RuntimeComponents extends Components
      implicit val argumentComponent: Argument[Components] = {
        case "enso"    => EnsoComponents.asRight
        case "runtime" => RuntimeComponents.asRight
        case other =>
          List(
            s"Unknown argument '$other' - expected 'enso', 'runtime' " +
            "or no argument to print a general summary."
          ).asLeft
      }

      val what = Opts.optionalArgument[Components](
        "components",
        "Can be either 'enso', 'runtime' or none. " +
        "If not specified, prints a summary of all installed components."
      )
      what map { what => () =>
        what match {
          case Some(EnsoComponents)    => println("List enso")
          case Some(RuntimeComponents) => println("List runtime")
          case None                    => println("List summary")
        }
      }
    }

  private def configCommand: Command =
    Command("config", "Modify project or user configuration.") {
      val global = Opts.flag(
        "global",
        "Set global user config. By default sets config of current project."
      )
      val key   = Opts.positionalArgument[String]("key")
      val value = Opts.positionalArgument[String]("value")
      (key, value, global) mapN { (key, value, global) => () =>
        val which = if (global) "global" else "local"
        println(s"Set in the $which config $key => $value")
      }
    }

  private def helpCommand: Command =
    Command("help", "Display summary of available commands.") {
      pure(()) map { _ => () => printTopLevelHelp() }
    }

  private def specialOptions: Opts[() => Boolean] = {
    val help    = Opts.flag("help", 'h', "Display help.")
    val version = Opts.flag("version", 'V', "Display version.")
    val json    = jsonFlag
    (help, version, json) mapN { (help, version, useJSON) => () =>
      if (help) {
        printTopLevelHelp()
        true
      } else if (version) {
        Launcher.displayVersion(useJSON)
        true
      } else false
    }
  }

  private def handleTopLevelOptions(args: Seq[String]): Boolean = {
    Opts.parse(specialOptions)(args) match {
      case Left(_) =>
        false
      case Right(handler) => handler()
    }
  }

  private def findPlugin(name: String): Boolean = name == "ide" // TODO [RW]

  private object LauncherPluginManager extends PluginManager {
    override def tryRunningPlugin(
      name: String,
      args: Seq[String]
    ): PluginBehaviour =
      findPlugin(name) match {
        case true =>
          PluginInterceptedFlow(() => {
            println(s"Would launch $name with $args") // TODO [RW] launch plugin
          })
        case false =>
          PluginNotFound
      }

    override def pluginsHelp(): Seq[CommandHelp] = {
      // TODO [RW] find plugins, --describe
      Seq(CommandHelp("ide", "Launch Enso IDE."))
    }

    override def pluginsNames(): Seq[String] = Seq("ide")
  }

  private val commandsConfig: Application =
    Application(
      "enso",
      "Enso Launcher",
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
      LauncherPluginManager
    )

  private def printTopLevelHelp(): Unit = {
    commandsConfig.displayHelp()
  }

  def main(args: Array[String]): Unit = {
    val arguments = args.toSeq
    if (!handleTopLevelOptions(arguments)) {
      commandsConfig.parse(arguments) match {
        case Left(errors) =>
          println(errors.mkString("\n"))
          System.exit(1)
        case Right(run) => run()
      }
    }
  }
}
