package org.enso.launcher

import java.nio.file.Path
import java.util.UUID

import org.enso.launcher.cli.{
  Application,
  Argument,
  CLIOutput,
  Command,
  CommandHelp,
  Opts,
  PluginBehaviour,
  PluginInterceptedFlow,
  PluginManager,
  PluginNotFound,
  TopLevelBehavior
}
import org.enso.launcher.cli.Opts._
import cats.implicits._

object Main {
  private def jsonFlag: Opts[Boolean] =
    Opts.flag("json", "Use JSON instead of plain text for output.")

  type Config = Unit

  private def versionCommand: Command[Config] =
    Command(
      "version",
      "Print version of the launcher and currently selected Enso distribution."
    ) {
      jsonFlag map { useJSON => (_: Config) =>
        Launcher.displayVersion(useJSON)
      }
    }

  private def newCommand: Command[Config] =
    Command("new", "Create a new Enso project.") {
      val nameOpt = Opts.positionalArgument[String]("NAME", "Project name.")
      val pathOpt = Opts.optionalArgument[Path](
        "PATH",
        "Path where to create the project. " +
        "By default a directory called <name> is created in the current directory."
      )

      (nameOpt, pathOpt) mapN { (name, path) => (_: Config) =>
        Launcher.newProject(name, path)
      }
    }

  private def jvmArgs =
    Opts.prefixedParameters(
      "jvm",
      "Parameters prefixed with jvm (--jvm.key=value) will be passed to the launched JVM as -Dkey=value."
    )

  private def runCommand: Command[Config] =
    Command("run", "Run a project or Enso script.") {
      val pathOpt        = Opts.optionalArgument[Path]("PATH")
      val additionalArgs = Opts.additionalArguments()
      (pathOpt, jvmArgs, additionalArgs) mapN {
        (path, jvmArgs, additionalArgs) => (_: Config) =>
          println(s"Launch runner for $path")
          println(s"JVM=$jvmArgs, additionalArgs=$additionalArgs")
      }
    }

  private def languageServerCommand: Command[Config] =
    Command(
      "language-server",
      "Launch the Language Server for a given project."
    ) {
      val rootId         = Opts.parameter[UUID]("root-id", "UUID", "TODO explain.")
      val path           = Opts.parameter[Path]("path", "PATH", "Project path.")
      val additionalArgs = Opts.additionalArguments()
      (rootId, path, jvmArgs, additionalArgs) mapN {
        (rootId, path, jvmArgs, additionalArgs) => (_: Config) =>
          println(s"Launch language server in $path with id=$rootId.")
          println(s"JVM=$jvmArgs, additionalArgs=$additionalArgs")
      }
    }

  private def replCommand: Command[Config] =
    Command("repl", "Launch an Enso REPL.") {
      val path           = Opts.optionalParameter[Path]("path", "PATH", "Project path.")
      val additionalArgs = Opts.additionalArguments()
      (path, jvmArgs, additionalArgs) mapN {
        (path, jvmArgs, additionalArgs) => (_: Config) =>
          println(s"Launch REPL in $path")
          println(s"JVM=$jvmArgs, additionalArgs=$additionalArgs")
      }
    }

  private def defaultCommand: Command[Config] =
    Command("default", "Print or change the default Enso version.") {
      val version = Opts.optionalArgument[String](
        "VERSION",
        "If provided, sets default version to VERSION. " +
        "Otherwise, current default is displayed."
      )
      version map { version => (_: Config) =>
        version match {
          case Some(version) => println(s"Set version to $version")
          case None          => println("Print current version")
        }
      }
    }

  private def upgradeCommand: Command[Config] =
    Command("upgrade", "Upgrade the launcher.") {
      val version = Opts.optionalArgument[String](
        "version",
        "Version to upgrade to. " +
        "If not provided, defaults to latest version."
      )
      version map { version => (_: Config) =>
        version match {
          case Some(version) => println(s"Upgrade launcher to $version")
          case None          => println("Upgrade launcher to latest version")
        }
      }
    }

  private def installCommand: Command[Config] =
    Command("install", "Install a selected Enso version.") {
      val version = Opts.positionalArgument[String]("VERSION")
      version map { version => (_: Config) =>
        println(s"Install $version")
      }
    }

  private def uninstallCommand: Command[Config] =
    Command("uninstall", "Uninstall an Enso version.") {
      val version = Opts.positionalArgument[String]("VERSION")
      version map { version => (_: Config) =>
        println(s"Uninstall $version")
      }
    }

  private def listCommand: Command[Config] =
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
        "<component>",
        "Can be either 'enso', 'runtime' or none. " +
        "If not specified, prints a summary of all installed components."
      )
      what map { what => (_: Config) =>
        what match {
          case Some(EnsoComponents)    => println("List enso")
          case Some(RuntimeComponents) => println("List runtime")
          case None                    => println("List summary")
        }
      }
    }

  private def configCommand: Command[Config] =
    Command("config", "Modify project or user configuration.") {
      val global = Opts.flag(
        "global",
        "Set global user config. By default sets config of current project."
      )
      val key   = Opts.positionalArgument[String]("KEY")
      val value = Opts.positionalArgument[String]("VALUE")
      (key, value, global) mapN { (key, value, global) => (_: Config) =>
        val which = if (global) "global" else "local"
        println(s"Set in the $which config $key => $value")
      }
    }

  private def helpCommand: Command[Config] =
    Command("help", "Display summary of available commands.") {
      pure(()) map { (_: Config) => (_: Config) => printTopLevelHelp() }
    }

  private def topLevelOpts: Opts[() => TopLevelBehavior[Config]] = {
    val help    = Opts.flag("help", 'h', "Display help.")
    val version = Opts.flag("version", 'V', "Display version.")
    val json    = jsonFlag
    (help, version, json) mapN { (help, version, useJSON) => () =>
      if (help) {
        printTopLevelHelp()
        TopLevelBehavior.Halt
      } else if (version) {
        Launcher.displayVersion(useJSON)
        TopLevelBehavior.Halt
      } else TopLevelBehavior.Continue(())
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

  private val application: Application[Config] =
    Application(
      "enso",
      "Enso Launcher",
      topLevelOpts,
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
    CLIOutput.println(application.displayHelp())
  }

  def main(args: Array[String]): Unit = {
    application.parse(args) match {
      case Left(errors) =>
        CLIOutput.println(errors.mkString("\n"))
        System.exit(1)
      case Right(()) =>
    }
  }
}
