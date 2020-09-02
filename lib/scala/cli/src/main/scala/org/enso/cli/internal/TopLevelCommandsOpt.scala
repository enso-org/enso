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
        pluginManager.get.tryRunningPlugin(
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
}
