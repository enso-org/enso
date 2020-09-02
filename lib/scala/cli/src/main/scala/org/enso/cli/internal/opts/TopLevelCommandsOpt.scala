package org.enso.cli.internal.opts

import cats.Semigroupal
import cats.data.NonEmptyList
import org.enso.cli.Opts.implicits._
import org.enso.cli._
import org.enso.cli.internal.{Parser, ParserContinuation}

/**
  * TODO
  */
class TopLevelCommandsOpt[A, B](
  toplevelOpts: Opts[A],
  commands: NonEmptyList[Command[B => Unit]],
  pluginManager: Option[PluginManager],
  helpHeader: String
) extends BaseSubcommandOpt[(A, Option[B => Unit]), B => Unit] {

  def helpOpt: Opts[Boolean] =
    Opts.flag("help", 'h', "Print this help message.", showInUsage = true)
  private val toplevelWithHelp =
    implicitly[Semigroupal[Opts]].product(toplevelOpts, helpOpt)

  override def availableSubcommands: NonEmptyList[Command[B => Unit]] = commands

  override def handleUnknownCommand(command: String): ParserContinuation = {
    val pluginAvailable = pluginManager.exists(_.hasPlugin(command))
    if (pluginAvailable) {
      ParserContinuation.Escape((remainingTokens, additionalArguments) =>
        pluginManager.get.runPlugin(
          command,
          Parser.untokenize(remainingTokens) ++ additionalArguments
        )
      )
    } else {
      addError(commandSuggestions(command))
      ParserContinuation.ContinueNormally
    }
  }

  override private[cli] def result(
    commandPrefix: Seq[String]
  ): Either[OptsParseError, (A, Option[B => Unit])] = {
    val prefix                 = extendPrefix(commandPrefix)
    val topLevelResultWithHelp = toplevelWithHelp.result(prefix)
    val topLevelResult         = topLevelResultWithHelp.map(_._1)
    val commandResult          = selectedCommand.map(_.opts.result(prefix))
    val result = (topLevelResultWithHelp, commandResult) match {
      case (Right((result, true)), _) =>
        def displayHelp(): Unit = {
          val helpText = renderHelp(commandPrefix)
          CLIOutput.println(helpText)
        }

        Right((result, Some((_: B) => displayHelp())))
      case (_, Some(value)) =>
        OptsParseError.product(topLevelResult, value.map(Some(_)))
      case (_, None) =>
        topLevelResult.map((_, None))
    }

    result.addErrors(errors.reverse).appendFullHelp(renderHelp(commandPrefix))
  }

  private def renderHelp(commandPrefix: Seq[String]): String =
    selectedCommand match {
      case Some(command) =>
        commandHelp(command, commandPrefix)
      case None =>
        topLevelHelp(commandPrefix)
    }

  /**
    * Generates a help text for an unknown command, including, if available,
    * suggestions of similar commands.
    *
    * @param typo the unrecognized command name
    */
  def commandSuggestions(typo: String): String = {
    val header =
      s"`$typo` is not a valid command."
    val plugins          = pluginManager.map(_.pluginsNames()).getOrElse(Seq())
    val possibleCommands = availableSubcommands.toList.map(_.name) ++ plugins
    val similar = Spelling.selectClosestMatches(
      typo,
      possibleCommands
    )
    val suggestions =
      if (similar.isEmpty) "\n\n" + availableCommands()
      else {
        "\n\nThe most similar commands are\n" +
        similar.map(CLIOutput.indent + _ + "\n").mkString
      }

    header + suggestions
  }

  override private[cli] def flags =
    super.flags ++ toplevelWithHelp.flags
  override private[cli] def parameters =
    super.parameters ++ toplevelWithHelp.parameters
  override private[cli] def prefixedParameters =
    super.prefixedParameters ++ toplevelWithHelp.prefixedParameters
  override private[cli] def gatherOptions =
    super.gatherOptions ++ toplevelWithHelp.gatherOptions
  override private[cli] def gatherPrefixedParameters =
    super.gatherPrefixedParameters ++ toplevelWithHelp.gatherPrefixedParameters
  override private[cli] def usageOptions =
    super.usageOptions ++ toplevelWithHelp.usageOptions

  // Note [Arguments in Top-Level Options]
  private def validateNoArguments(): Unit = {
    val topLevelHasArguments =
      toplevelWithHelp.requiredArguments.nonEmpty ||
      toplevelWithHelp.optionalArguments.nonEmpty ||
      toplevelWithHelp.trailingArguments.nonEmpty ||
      toplevelWithHelp.additionalArguments.nonEmpty
    if (topLevelHasArguments) {
      throw new IllegalArgumentException(
        "Internal error: " +
        "The top level options are not allowed to take arguments."
      )
    }
  }
  validateNoArguments()

  override private[cli] def reset(): Unit = {
    super.reset()
    toplevelWithHelp.reset()
  }

  override def availableOptionsHelp(): Seq[String] = {
    val r =
      super.availableOptionsHelp() ++ toplevelWithHelp.availableOptionsHelp()
    println(r)
    r
  }

  override def availablePrefixedParametersHelp(): Seq[String] =
    super.availablePrefixedParametersHelp() ++
    toplevelWithHelp.availablePrefixedParametersHelp()

  override def additionalHelp(): Seq[String] =
    super.additionalHelp() ++ toplevelWithHelp.additionalHelp()

  override def commandLines(
    alwaysIncludeOtherOptions: Boolean = false
  ): NonEmptyList[String] = {
    val include =
      alwaysIncludeOtherOptions || toplevelWithHelp.gatherOptions.nonEmpty
    super.commandLines(alwaysIncludeOtherOptions = include)
  }

  def commandHelp(command: Command[_], commandPrefix: Seq[String]): String = {
    val applicationName = commandPrefix.head
    val mergedOpts =
      implicitly[Semigroupal[Opts]].product(command.opts, toplevelWithHelp)
    command.comment + "\n" + mergedOpts.help(Seq(applicationName, command.name))
  }

  def availableCommands(): String = {
    val pluginsHelp = pluginManager.map(_.pluginsHelp()).getOrElse(Seq())
    val subCommands = commands.toList.map(_.topLevelHelp) ++ pluginsHelp
    val commandDescriptions =
      subCommands.map(_.toString).map(CLIOutput.indent + _ + "\n").mkString
    "Available commands:\n" + commandDescriptions
  }

  def topLevelHelp(commandPrefix: Seq[String]): String = {
    val usageOptions = toplevelWithHelp
      .commandLineOptions(alwaysIncludeOtherOptions = false)
      .stripLeading()
    val commandName = commandPrefix.head
    val usage =
      s"Usage: $commandName\t$usageOptions COMMAND [ARGS]\n"

    val topLevelOptionsHelp =
      toplevelWithHelp.helpExplanations(addHelpOption = false)

    val sb = new StringBuilder
    sb.append(helpHeader + "\n")
    sb.append(usage)
    sb.append("\n" + availableCommands())
    sb.append(topLevelOptionsHelp)
    sb.append(
      s"\nFor more information on a specific command listed above," +
      s" please run `$commandName COMMAND --help`."
    )

    sb.toString()
  }
}

/*
 * Note [Arguments in Top-Level Options]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * We do not override the functions handling arguments, because we just need to
 * handle arguments of the subcommand. By definition, the top level options
 * cannot include arguments.
 */
