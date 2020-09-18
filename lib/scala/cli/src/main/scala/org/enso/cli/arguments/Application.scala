package org.enso.cli.arguments

import cats.data.NonEmptyList
import org.enso.cli.internal.Parser
import org.enso.cli.internal.opts.TopLevelCommandsOpt

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
  val commands: NonEmptyList[Command[Config => Int]],
  val pluginManager: Option[PluginManager]
) {

  private val combinedOpts =
    new TopLevelCommandsOpt(
      topLevelOpts,
      commands,
      pluginManager,
      helpHeader
    )

  /**
    * A helper overload that accepts the array as provided to the main function.
    */
  def run(
    args: Array[String]
  ): Either[List[String], Int] = run(args.toSeq)

  /**
    * Runs the application logic. Parses the top level options and depending on
    * its result, possibly runs a command or a plugin.
    *
    * If a plugin is run, this function does not return (the application exits
    * with the exit code returned by the plugin).
    *
    * @return either a list of errors encountered when parsing the options or
    *         the exit code if succeeded
    */
  def run(
    args: Seq[String]
  ): Either[List[String], Int] = {
    val (tokens, additionalArguments) = Parser.tokenize(args)
    val parseResult =
      Parser.parseOpts(
        combinedOpts,
        tokens,
        additionalArguments,
        applicationName = commandName
      )
    val finalResult = parseResult.flatMap {
      case ((topLevelAction, commandResult), pluginIntercepted) =>
        pluginIntercepted match {
          case Some(pluginHandler) =>
            Right(pluginHandler())
          case None =>
            val topLevelBehavior = topLevelAction()
            topLevelBehavior match {
              case TopLevelBehavior.Halt(exitCode) =>
                Right(exitCode)
              case TopLevelBehavior.Continue(config) =>
                commandResult match {
                  case Some(action) =>
                    Right(action(config))
                  case None =>
                    Left(OptsParseError("Expected a command.", renderHelp()))
                }
            }
        }
    }
    finalResult.toErrorList
  }

  /**
    * Generates a help text summarizing the usage of the application and listing
    * available commands and top-level options.
    */
  def renderHelp(): String = combinedOpts.topLevelHelp(Seq(commandName))
}

object Application {

  /**
    * Helper constructor for [[Application]].
    *
    * @param commandName default name of the application for use in commands
    * @param prettyName pretty name of the application for use in text
    * @param helpHeader short description of the application included at the
    *                   top of the help text
    * @param topLevelOpts the top-level options that are used to parse a global
    *                     config which is passed to every command; can also be
    *                     used to execute different bahavior than commands; see
    *                     [[TopLevelBehavior]] for more information
    * @param commands a sequence of commands supported by the application
    * @param pluginManager a plugin manager for resolving non-native command
    *                      extensions
    */
  def apply[Config](
    commandName: String,
    prettyName: String,
    helpHeader: String,
    topLevelOpts: Opts[() => TopLevelBehavior[Config]],
    commands: NonEmptyList[Command[Config => Int]],
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

  /**
    * Helper constructor for [[Application]].
    *
    * Creates an application without plugin support.
    *
    * @param commandName default name of the application for use in commands
    * @param prettyName pretty name of the application for use in text
    * @param helpHeader short description of the application included at the
    *                   top of the help text
    * @param topLevelOpts the top-level options that are used to parse a global
    *                     config which is passed to every command; can also be
    *                     used to execute different bahavior than commands; see
    *                     [[TopLevelBehavior]] for more information
    * @param commands a sequence of commands supported by the application
    */
  def apply[Config](
    commandName: String,
    prettyName: String,
    helpHeader: String,
    topLevelOpts: Opts[() => TopLevelBehavior[Config]],
    commands: NonEmptyList[Command[Config => Int]]
  ): Application[Config] =
    new Application(
      commandName,
      prettyName,
      helpHeader,
      topLevelOpts,
      commands,
      None
    )

  /**
    * Helper constructor for [[Application]].
    *
    * Creates an application without any top-level options and without plugin
    * support.
    *
    * @param commandName default name of the application for use in commands
    * @param prettyName pretty name of the application for use in text
    * @param helpHeader short description of the application included at the
    *                   top of the help text
    * @param commands a sequence of commands supported by the application
    */
  def apply(
    commandName: String,
    prettyName: String,
    helpHeader: String,
    commands: NonEmptyList[Command[Unit => Int]]
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
