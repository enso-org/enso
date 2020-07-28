package org.enso.launcher

import java.nio.file.Path
import java.util.UUID

import org.enso.cli.{
  Application,
  Argument,
  CLIOutput,
  Command,
  Opts,
  Subcommand,
  TopLevelBehavior
}
import org.enso.cli.Opts.implicits._
import cats.implicits._
import org.enso.launcher.installation.{
  DistributionInstaller,
  DistributionManager
}
import org.enso.launcher.installation.DistributionInstaller.BundleAction

object Main {
  private def jsonFlag(showInUsage: Boolean): Opts[Boolean] =
    Opts.flag(
      "json",
      "Use JSON instead of plain text for version output.",
      showInUsage
    )

  type Config = Unit

  private def versionCommand: Command[Config => Unit] =
    Command(
      "version",
      "Print version of the launcher and currently selected Enso distribution."
    ) {
      jsonFlag(showInUsage = true) map { useJSON => (_: Config) =>
        Launcher.displayVersion(useJSON)
      }
    }

  private def newCommand: Command[Config => Unit] =
    Command("new", "Create a new Enso project.", related = Seq("create")) {
      val nameOpt = Opts.positionalArgument[String]("NAME", "Project name.")
      val pathOpt = Opts.optionalArgument[Path](
        "PATH",
        "Path where to create the project. If not specified, a directory " +
        "called NAME is created in the current directory."
      )

      (nameOpt, pathOpt) mapN { (name, path) => (_: Config) =>
        Launcher.newProject(name, path)
      }
    }

  private def jvmArgs =
    Opts.prefixedParameters(
      "jvm",
      "These parameters will be passed to the launched JVM as -DKEY=VALUE."
    )

  private def runCommand: Command[Config => Unit] =
    Command(
      "run",
      "Run a project or Enso script.",
      related = Seq("exec", "execute", "build")
    ) {
      val pathOpt = Opts.optionalArgument[Path](
        "PATH",
        "If PATH points to a file, that file is run as a script. " +
        "If it points to a directory, the project from that directory is " +
        "run. If a PATH is not provided, a project in the current working " +
        "directory is run."
      )
      val additionalArgs = Opts.additionalArguments()
      (pathOpt, jvmArgs, additionalArgs) mapN {
        (path, jvmArgs, additionalArgs) => (_: Config) =>
          val enginesRoot = DistributionManager.paths.engines
          println(s"Launch runner for $path")
          println(s"JVM=$jvmArgs, additionalArgs=$additionalArgs")
          println(s"Engines are located at $enginesRoot")
      }
    }

  private def languageServerCommand: Command[Config => Unit] =
    Command(
      "language-server",
      "Launch the Language Server for a given project.",
      related = Seq("server")
    ) {
      val rootId = Opts.parameter[UUID]("root-id", "UUID", "Content root id.")
      val path =
        Opts.parameter[Path]("path", "PATH", "Path to the content root.")
      val interface =
        Opts.optionalParameter[String](
          "interface",
          "INTERFACE",
          "Interface for processing all incoming connections."
        )
      val rpcPort =
        Opts.optionalParameter[Int](
          "rpc-port",
          "PORT",
          "RPC port for processing all incoming connections."
        )
      val dataPort =
        Opts.optionalParameter[Int](
          "data-port",
          "PORT",
          "Data port for visualisation protocol."
        )
      val additionalArgs = Opts.additionalArguments()
      (
        rootId,
        path,
        interface,
        rpcPort,
        dataPort,
        jvmArgs,
        additionalArgs
      ) mapN {
        (rootId, path, interface, rpcPort, dataPort, jvmArgs, additionalArgs) =>
          (_: Config) =>
            println(s"Launch language server in $path with id=$rootId.")
            println(
              s"interface=$interface, rpcPort=$rpcPort, dataPort=$dataPort"
            )
            println(s"JVM=$jvmArgs, additionalArgs=$additionalArgs")
      }
    }

  private def replCommand: Command[Config => Unit] =
    Command("repl", "Launch an Enso REPL.") {
      val path           = Opts.optionalParameter[Path]("path", "PATH", "Project path.")
      val additionalArgs = Opts.additionalArguments()
      (path, jvmArgs, additionalArgs) mapN {
        (path, jvmArgs, additionalArgs) => (_: Config) =>
          println(s"Launch REPL in $path")
          println(s"JVM=$jvmArgs, additionalArgs=$additionalArgs")
      }
    }

  private def defaultCommand: Command[Config => Unit] =
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

  private def upgradeCommand: Command[Config => Unit] =
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

  private def installEngineCommand: Subcommand[Config => Unit] =
    Subcommand("engine") {
      val version = Opts.positionalArgument[String]("VERSION")
      version map { version => (_: Config) =>
        println(s"Install $version")
      }
    }

  private def installDistributionCommand: Subcommand[Config => Unit] =
    Subcommand("distribution") {
      val autoConfirm = Opts.flag(
        "auto-confirm",
        "Proceeds with installation without asking confirmation questions. " +
        "If bundled components are present, this flag will move them by " +
        "default, unless overridden by an explicit setting of " +
        "`--install-bundle-mode`. On success, the installer will remove " +
        "itself to avoid conflicts with the installed launcher executable.",
        showInUsage = false
      )
      implicit val bundleActionParser: Argument[BundleAction] = {
        case "move"   => DistributionInstaller.MoveBundles.asRight
        case "copy"   => DistributionInstaller.CopyBundles.asRight
        case "ignore" => DistributionInstaller.IgnoreBundles.asRight
        case other =>
          List(
            s"`$other` is not a valid bundle-install-mode value. " +
            s"Possible values are: `move`, `copy`, `ignore`."
          ).asLeft
      }
      val bundleAction = Opts.optionalParameter[BundleAction](
        "bundle-install-mode",
        "(move | copy | ignore)",
        "Specifies how bundled engines and runtimes should be treated. " +
        "If `auto-confirm` is set, defaults to move.",
        showInUsage = false
      )
      (autoConfirm, bundleAction) mapN {
        (autoConfirm, bundleAction) => (_: Config) =>
          new DistributionInstaller(
            DistributionManager,
            autoConfirm,
            if (autoConfirm)
              Some(bundleAction.getOrElse(DistributionInstaller.MoveBundles))
            else bundleAction
          ).install()
      }
    }

  private def installCommand: Command[Config => Unit] =
    Command(
      "install",
      "Install a new version of engine or install the distribution locally."
    ) {
      Opts.subcommands(installEngineCommand, installDistributionCommand)
    }

  private def uninstallCommand: Command[Config => Unit] =
    Command("uninstall", "Uninstall an Enso version.") {
      val version = Opts.positionalArgument[String]("VERSION")
      version map { version => (_: Config) =>
        println(s"Uninstall $version")
      }
    }

  private def listCommand: Command[Config => Unit] =
    Command("list", "List installed components.") {
      sealed trait Components
      case object EnsoComponents    extends Components
      case object RuntimeComponents extends Components
      implicit val argumentComponent: Argument[Components] = {
        case "enso"    => EnsoComponents.asRight
        case "runtime" => RuntimeComponents.asRight
        case other =>
          List(
            s"Unknown argument `$other` - expected `enso`, `runtime` " +
            "or no argument to print a general summary."
          ).asLeft
      }

      val what = Opts.optionalArgument[Components](
        "COMPONENT",
        "COMPONENT can be either `enso`, `runtime` or none. " +
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

  private def configCommand: Command[Config => Unit] =
    Command("config", "Modify project or user configuration.") {
      val global = Opts.flag(
        "global",
        "Set global user config. By default sets config of current project.",
        showInUsage = true
      )
      val key   = Opts.positionalArgument[String]("KEY")
      val value = Opts.positionalArgument[String]("VALUE")
      (key, value, global) mapN { (key, value, global) => (_: Config) =>
        val which = if (global) "global" else "local"
        println(s"Set in the $which config $key => $value")
      }
    }

  private def helpCommand: Command[Config => Unit] =
    Command("help", "Display summary of available commands.") {
      Opts.pure(()) map { _ => (_: Config) => printTopLevelHelp() }
    }

  private def topLevelOpts: Opts[() => TopLevelBehavior[Config]] = {
    val help = Opts.flag("help", 'h', "Display help.", showInUsage = true)
    val version =
      Opts.flag("version", 'V', "Display version.", showInUsage = true)
    val json = jsonFlag(showInUsage = false)
    val ensurePortable = Opts.flag(
      "ensure-portable",
      "Ensures that the launcher is run in portable mode.",
      showInUsage = false
    )
    val internalOpts = InternalOpts.topLevelOptions

    (internalOpts, help, version, json, ensurePortable) mapN {
      (_, help, version, useJSON, shouldEnsurePortable) => () =>
        if (shouldEnsurePortable) {
          Launcher.ensurePortable()
        }

        if (help) {
          printTopLevelHelp()
          TopLevelBehavior.Halt
        } else if (version) {
          Launcher.displayVersion(useJSON)
          TopLevelBehavior.Halt
        } else TopLevelBehavior.Continue(())
    }
  }

  private val application: Application[Config] =
    Application(
      "enso",
      "Enso",
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
      PluginManager
    )

  private def printTopLevelHelp(): Unit = {
    CLIOutput.println(application.renderHelp())
  }

  def main(args: Array[String]): Unit = {
    try {
      application.run(args) match {
        case Left(errors) =>
          CLIOutput.println(errors.mkString("\n"))
          sys.exit(1)
        case Right(()) =>
      }
    } catch {
      case e: RuntimeException =>
        Logger.error(s"A fatal error has occurred: $e", e)
    }
  }
}
