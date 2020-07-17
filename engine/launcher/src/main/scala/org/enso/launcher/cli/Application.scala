package org.enso.launcher.cli

import org.enso.launcher.cli.internal.Parser

sealed trait PluginBehaviour
case object PluginNotFound                        extends PluginBehaviour
case class PluginInterceptedFlow(run: () => Unit) extends PluginBehaviour
trait PluginManager {
  def tryRunningPlugin(name: String, args: Seq[String]): PluginBehaviour
  def pluginsNames():                                    Seq[String]
  def pluginsHelp():                                     Seq[CommandHelp]
}

trait TopLevelBehavior[+Config]
object TopLevelBehavior {
  case class Continue[Config](withConfig: Config)
      extends TopLevelBehavior[Config]
  case object Halt extends TopLevelBehavior[Nothing]
}

class Application[Config](
  val commandName: String,
  val prettyName: String,
  val helpHeader: String,
  val topLevelOpts: Opts[() => TopLevelBehavior[Config]],
  val commands: Seq[Command[Config => Unit]],
  val pluginManager: Option[PluginManager]
) {
  def run(
    args: Array[String]
  ): Either[List[String], Unit] = run(args.toSeq)

  def run(
    args: Seq[String]
  ): Either[List[String], Unit] = {
    val (tokens, additionalArguments) = Parser.tokenize(args)
    val topLevelParseResult =
      Parser.parseOpts(topLevelOpts, tokens, Seq(), isTopLevel = true)
    topLevelParseResult.flatMap {
      case (run, restOfTokens) =>
        run() match {
          case TopLevelBehavior.Halt => Right(())
          case TopLevelBehavior.Continue(config) =>
            val subCommandResult = Parser.parseSubcommand(
              this,
              config,
              restOfTokens,
              additionalArguments
            )
            subCommandResult.map { run =>
              run()
            }
        }
    }
  }

  def gatherCommandNames(): Seq[String] =
    commands.map(_.name) ++ pluginManager.map(_.pluginsNames()).getOrElse(Seq())

  def displayHelp(): String = {
    val usageOptions = topLevelOpts.commandLineOptions()
    val usage        = s"Usage: $commandName\t${usageOptions}COMMAND [ARGS]\n"

    val subCommands = commands.map(_.topLevelHelp) ++ pluginManager
        .map(_.pluginsHelp())
        .getOrElse(Seq())
    val commandDescriptions =
      subCommands.map(_.toString).map("    " + _).mkString("\n")

    val topLevelOptions =
      topLevelOpts.helpExplanations().map("    " + _).mkString("\n")
    val topLevelOptionsHelp =
      if (topLevelOptions.isEmpty) ""
      else "\nAvailable options:\n" + topLevelOptions + "\n"

    val sb = new StringBuilder
    sb.append(helpHeader + "\n")
    sb.append(usage)
    sb.append("\nAvailable commands:\n")
    sb.append(commandDescriptions + "\n")
    sb.append(topLevelOptionsHelp)
    sb.append(
      s"\nFor more information on a specific command listed above," +
      s" please run `$commandName COMMAND --help`."
    )

    sb.toString()
  }
}

object Application {
  // TODO [RW] FIXME names
  def apply[Config](
    name: String,
    helpHeader: String,
    topLevelOpts: Opts[() => TopLevelBehavior[Config]],
    commands: Seq[Command[Config => Unit]],
    pluginManager: PluginManager
  ): Application[Config] =
    new Application(
      name,
      name,
      helpHeader,
      topLevelOpts,
      commands,
      Some(pluginManager)
    )

  def apply[Config](
    name: String,
    helpHeader: String,
    topLevelOpts: Opts[() => TopLevelBehavior[Config]],
    commands: Seq[Command[Config => Unit]]
  ): Application[Config] =
    new Application(name, name, helpHeader, topLevelOpts, commands, None)

  def apply(
    name: String,
    helpHeader: String,
    commands: Seq[Command[Unit => Unit]]
  ): Application[()] =
    new Application(
      name,
      name,
      helpHeader,
      Opts.pure { () => TopLevelBehavior.Continue(()) },
      commands,
      None
    )
}
