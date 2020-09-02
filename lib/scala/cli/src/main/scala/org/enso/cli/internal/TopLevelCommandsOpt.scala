package org.enso.cli.internal

import cats.data.NonEmptyList
import org.enso.cli._

/**
  * TODO
  * @param toplevelOpts
  * @param commands
  * @tparam A
  * @tparam B
  */
class TopLevelCommandsOpt[A, B](
  toplevelOpts: Opts[A],
  commands: NonEmptyList[Command[B => Unit]],
  pluginManager: Option[PluginManager]
) extends BaseSubcommandOpt[(A, Option[B => Unit]), B => Unit] {

  override def availableSubcommands: NonEmptyList[Command[B => Unit]] = commands

  override def handleUnknownCommand(
    command: String,
    commandPrefix: Seq[String]
  ): ParserContinuation = {
    val pluginAvailable = pluginManager.exists(_.hasPlugin(command))
    if (pluginAvailable) {
      ParserContinuation.Escape((remainingTokens, additionalArguments) =>
        pluginManager.get.runPlugin(
          command,
          Parser.untokenize(remainingTokens) ++ additionalArguments
        )
      )
    } else {
      addError(commandSuggestions(command, commandPrefix))
      ParserContinuation.ContinueNormally
    }
  }

  override private[cli] def result(
    commandPrefix: Seq[String]
  ): Either[OptsParseError, (A, Option[B => Unit])] = {
    val topLevelResult = toplevelOpts.result(commandPrefix)
    val commandResult  = selectedCommand.map(_.opts.result(commandPrefix))
    val result = commandResult match {
      case Some(value) =>
        OptsParseError.product(topLevelResult, value.map(Some(_)))
      case None =>
        topLevelResult.map((_, None))
    }

    OptsParseError.addErrors(result, errors.reverse)
  }

  /**
    * Generates a help text for an unknown command, including, if available,
    * suggestions of similar commands.
    *
    * @param typo the unrecognized command name
    */
  def commandSuggestions(typo: String, commandPrefix: Seq[String]): String = {
    val header =
      s"`$typo` is not a valid command. See " +
      s"`${commandPrefix.mkString(" ")} --help`.\n\n"
    val similar = Spelling.selectClosestMatches(
      typo,
      availableSubcommands.toList.map(_.name)
    )
    val suggestions =
      if (similar.isEmpty) ""
      else {
        "The most similar commands are\n" +
        similar.map(CLIOutput.indent + _ + "\n").mkString
      }

    header + suggestions
  }

  override private[cli] def flags =
    super.flags ++ toplevelOpts.flags
  override private[cli] def parameters =
    super.parameters ++ toplevelOpts.parameters
  override private[cli] def prefixedParameters =
    super.prefixedParameters ++ toplevelOpts.prefixedParameters
  override private[cli] def gatherOptions =
    super.gatherOptions ++ toplevelOpts.gatherOptions
  override private[cli] def gatherPrefixedParameters =
    super.gatherPrefixedParameters ++ toplevelOpts.gatherPrefixedParameters
  override private[cli] def usageOptions =
    super.usageOptions ++ toplevelOpts.usageOptions

  // Note [Arguments in Top-Level Options]
  private def validateNoArguments(): Unit = {
    val topLevelHasArguments =
      toplevelOpts.requiredArguments.nonEmpty ||
      toplevelOpts.optionalArguments.nonEmpty ||
      toplevelOpts.trailingArguments.nonEmpty ||
      toplevelOpts.additionalArguments.nonEmpty
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
    toplevelOpts.reset()
  }

  override def availableOptionsHelp(): Seq[String] =
    super.availableOptionsHelp() ++ toplevelOpts.availableOptionsHelp()

  override def availablePrefixedParametersHelp(): Seq[String] =
    super.availablePrefixedParametersHelp() ++
    toplevelOpts.availablePrefixedParametersHelp()

  override def additionalHelp(): Seq[String] =
    super.additionalHelp() ++ toplevelOpts.additionalHelp()

  override def commandLines(
    alwaysIncludeOtherOptions: Boolean = false
  ): NonEmptyList[String] = super.commandLines(alwaysIncludeOtherOptions = true)
}

/*
 * Note [Arguments in Top-Level Options]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * We do not override the functions handling arguments, because we just need to
 * handle arguments of the subcommand. By definition, the top level options
 * cannot include arguments.
 */
