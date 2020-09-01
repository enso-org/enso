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
  commands: NonEmptyList[Command[B]],
  pluginManager: Option[PluginManager]
) extends BaseSubcommandOpt[(A, Option[B]), B] {

  override def availableSubcommands: NonEmptyList[Command[B]] = commands

  override def handleUnknownCommand(
    command: String,
    commandPrefix: Seq[String]
  ): ParserContinuation = {
    ParserContinuation.Escape(tryHandlingPlugin(command, commandPrefix))
  }

  def tryHandlingPlugin(command: String, commandPrefix: Seq[String])(
    remainingTokens: Seq[Token],
    additionalArguments: Seq[String]
  ): Either[OptsParseError, Unit] = {
    val pluginBehaviour = pluginManager
      .map(
        _.tryRunningPlugin(
          command,
          Parser.untokenize(remainingTokens) ++ additionalArguments
        )
      )
      .getOrElse(PluginNotFound)
    pluginBehaviour match {
      case PluginNotFound =>
        OptsParseError.left(commandSuggestions(command, commandPrefix))
      case PluginInterceptedFlow => Right(())
    }
  }

  override private[cli] def result(
    commandPrefix: Seq[String]
  ): Either[OptsParseError, (A, Option[B])] = {
    // TODO combine errors from errors
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
