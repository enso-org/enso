package org.enso.cli

import org.enso.cli.internal.Parser

/**
  * Represents a CLI application with multiple commands.
  *
  * The top-level options can be used to parse global configuration that is
  * passed to all commands. It can also be used to override behavior (to execute
  * some other code rather than one of the commands). The top-level options
  * return a function `() => TopLevelBehaviour[Config]`. When parsed
  * successfully, that function is executed. It can trigger arbitrary
  * side-effects. It also defines the further behaviour of the application. If
  * it returns a [[TopLevelBehavior.Halt]], the application does not parse any
  * further commands but halts. If it returns [[TopLevelBehavior.Continue]],
  * parsing continues, trying to execute one of the subcommands. The `Config` is
  * passed to the subcommands as described below.
  *
  * When a command is entered, first its name is matched agains one of the
  * provided commands. If there is a match, that command is further parsed and
  * executed. Otherwise, if the name matches one of the related command names,
  * an error message is returned pointing the user at the right command. If no
  * related command matches, the plugin manager (if it is provided) is asked to
  * execute a plugin. If that also fails, an error message is displayed that the
  * command is not recognized. The message contains similar command names if
  * available.
  *
  * @param commandName default name of the application for use in commands
  * @param prettyName pretty name of the application for use in text
  * @param helpHeader short description of the application included at the top
  *                   of the help text
  * @param topLevelOpts the top-level options that are used to parse a global
  *                     config which is passed to every command; can also be
  *                     used to execute different bahavior than commands; see
  *                     [[TopLevelBehavior]] for more information
  * @param commands a sequence of commands supported by the application
  * @param pluginManager a plugin manager for resolving non-native command
  *                      extensions
  * @tparam Config type of configuration that is parsed by the top level
  *                options and passed to the commands
  */
class Application[Config](
  val commandName: String,
  val prettyName: String,
  val helpHeader: String,
  val topLevelOpts: Opts[() => TopLevelBehavior[Config]],
  val commands: Seq[Command[Config => Unit]],
  val pluginManager: Option[PluginManager]
) {

  /**
    * A helper overload that accepts the array as provided to the main function.
    */
  def run(
    args: Array[String]
  ): Either[List[String], Unit] = run(args.toSeq)

  /**
    * Runs the application logic. Parses the top level options and depending on
    * its result, possibly runs a command or a plugin.
    *
    * @return either a list of errors encountered when parsing the options or
    *         [[Unit]] if it succeeded
    */
  def run(
    args: Seq[String]
  ): Either[List[String], Unit] = {
    val (tokens, additionalArguments) = Parser.tokenize(args)
    val topLevelParseResult =
      Parser.parseOpts(
        topLevelOpts,
        tokens,
        Seq(),
        isTopLevel    = true,
        commandPrefix = Seq(commandName)
      )
    topLevelParseResult.flatMap {
      case (run, restOfTokens) =>
        run() match {
          case TopLevelBehavior.Halt => Right(())
          case TopLevelBehavior.Continue(config) =>
            val subCommandResult = Parser.parseCommand(
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

  /**
    * Generates a help text summarizing the usage of the application and listing
    * available commands and top-level options.
    */
  def renderHelp(): String = {
    val usageOptions = topLevelOpts.commandLineOptions().stripLeading()
    val usage        = s"Usage: $commandName\t${usageOptions} COMMAND [ARGS]\n"

    val subCommands = commands.map(_.topLevelHelp) ++ pluginManager
        .map(_.pluginsHelp())
        .getOrElse(Seq())
    val commandDescriptions =
      subCommands.map(_.toString).map(CLIOutput.indent + _ + "\n").mkString

    val topLevelOptionsHelp =
      topLevelOpts.helpExplanations(addHelpOption = false)

    val sb = new StringBuilder
    sb.append(helpHeader + "\n")
    sb.append(usage)
    sb.append("\nAvailable commands:\n")
    sb.append(commandDescriptions)
    sb.append(topLevelOptionsHelp)
    sb.append(
      s"\nFor more information on a specific command listed above," +
      s" please run `$commandName COMMAND --help`."
    )

    sb.toString()
  }

  /**
    * Generates a help text for an unknown command, including, if available,
    * suggestions of similar commands.
    *
    * @param typo the unrecognized command name
    */
  def commandSuggestions(typo: String): String = {
    val header =
      s"`$typo` is not a valid $prettyName command. See " +
      s"`$commandName --help`.\n\n"
    val similar = Spelling.selectClosestMatches(typo, gatherCommandNames())
    val suggestions =
      if (similar.isEmpty) ""
      else {
        "The most similar commands are\n" +
        similar.map(CLIOutput.indent + _ + "\n").mkString
      }

    header + suggestions
  }

  private def gatherCommandNames(): Seq[String] =
    commands.map(_.name) ++ pluginManager.map(_.pluginsNames()).getOrElse(Seq())
}

/**
  * The datatype returned by the [[PluginManager]], specifies whether a plugin
  * has been executed.
  */
sealed trait PluginBehaviour

/**
  * Returned if the plugin was not found.
  */
case object PluginNotFound extends PluginBehaviour

/**
  * Returned if a plugin has been found and has been run.
  */
case object PluginInterceptedFlow extends PluginBehaviour

/**
  * A plugin manager that handles finding and running plugins.
  */
trait PluginManager {

  /**
    * Tries to run a given plugin with provided arguments.
    *
    * @param name name of the plugin
    * @param args arguments that should be passed to it
    */
  def tryRunningPlugin(name: String, args: Seq[String]): PluginBehaviour

  /**
    * Lists names of plugins found in the system.
    */
  def pluginsNames(): Seq[String]

  /**
    * Lists names and short descriptions of plugins found on the system.
    */
  def pluginsHelp(): Seq[CommandHelp]
}

/**
  * Defines the behaviour of parsing top-level application options.
  *
  * @tparam Config type of configuration that is passed to commands
  */
trait TopLevelBehavior[+Config]
object TopLevelBehavior {

  /**
    * If top-level options return a value of this class, the application should
    * continue execution. The provided value of `Config` should be passed to the
    * executed commands.
    *
    * @param withConfig the configuration that is passed to commands
    * @tparam Config type of configuration that is passed to commands
    */
  case class Continue[Config](withConfig: Config)
      extends TopLevelBehavior[Config]

  /**
    * If top-level options return a value of this class, it means that the
    * top-level options have handled the execution and commands should not be
    * parsed further. This can be useful to implement top-level options like
    * `--version`.
    */
  case object Halt extends TopLevelBehavior[Nothing]
}

object Application {
  def apply[Config](
    commandName: String,
    prettyName: String,
    helpHeader: String,
    topLevelOpts: Opts[() => TopLevelBehavior[Config]],
    commands: Seq[Command[Config => Unit]],
    pluginManager: PluginManager
  ): Application[Config] =
    new Application(
      commandName,
      prettyName,
      helpHeader,
      topLevelOpts,
      commands,
      Some(pluginManager)
    )

  def apply[Config](
    commandName: String,
    prettyName: String,
    helpHeader: String,
    topLevelOpts: Opts[() => TopLevelBehavior[Config]],
    commands: Seq[Command[Config => Unit]]
  ): Application[Config] =
    new Application(
      commandName,
      prettyName,
      helpHeader,
      topLevelOpts,
      commands,
      None
    )

  def apply(
    commandName: String,
    prettyName: String,
    helpHeader: String,
    commands: Seq[Command[Unit => Unit]]
  ): Application[()] =
    new Application(
      commandName,
      prettyName,
      helpHeader,
      Opts.pure { () => TopLevelBehavior.Continue(()) },
      commands,
      None
    )
}
