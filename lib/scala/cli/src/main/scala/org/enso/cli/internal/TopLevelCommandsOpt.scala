package org.enso.cli.internal

import cats.data.NonEmptyList
import org.enso.cli.internal.Parser.{singleError, untokenize}
import org.enso.cli.{
  CLIOutput,
  Command,
  Opts,
  OptsParseError,
  PluginInterceptedFlow,
  PluginManager,
  PluginNotFound,
  Spelling,
  Subcommand
}

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

  override def handleUnknownCommand(command: String): ParserContinuation = {
    ParserContinuation.Escape(tryHandlingPlugin(command))
  }

  def tryHandlingPlugin(command: String)(
    remainingTokens: Seq[Token],
    additionalArguments: Seq[String]
  ): Either[List[String], Unit] = {
    val pluginBehaviour = pluginManager
      .map(
        _.tryRunningPlugin(
          command,
          untokenize(remainingTokens) ++ additionalArguments
        )
      )
      .getOrElse(PluginNotFound)
    pluginBehaviour match {
      case PluginNotFound =>
        ??? // TODO command suggestions
      case PluginInterceptedFlow => Right(())
    }
  }

  override private[cli] def result(
    commandPrefix: Seq[String]
  ): Either[List[String], (A, Option[B])] = {
    // TODO combine errors from errors
    val topLevelResult = toplevelOpts.result(commandPrefix)
    val commandResult  = selectedCommand.map(_.opts.result(commandPrefix))
    commandResult match {
      case Some(value) =>
        OptsParseError.product(topLevelResult, value.map(Some(_)))
      case None =>
        topLevelResult.map((_, None))
    }
//    if (errors.nonEmpty)
//      Left(errors.reverse)
//    else
//      selectedCommand match {
//        case Some(command) => command.opts.result(commandPrefix)
//        case None =>
//
//      }
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
