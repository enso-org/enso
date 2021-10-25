package org.enso.cli.internal.opts

import cats.data.NonEmptyList
import org.enso.cli.arguments.{Command, OptsParseError}
import org.enso.cli.internal.ParserContinuation
import org.enso.cli.{CLIOutput, Spelling}

class SubcommandOpt[A](subcommands: NonEmptyList[Command[A]])
    extends BaseSubcommandOpt[A, A] {

  /** @inheritdoc
    */
  override def availableSubcommands: NonEmptyList[Command[A]] = subcommands

  /** A flag that can be set to indicate that a command was provided, but it was
    * invalid.
    *
    * It is used for error reporting to differentiate invalid commands from
    * missing commands.
    */
  private var commandProvidedButInvalid: Boolean = false

  /** @inheritdoc
    */
  override def handleUnknownCommand(command: String): ParserContinuation = {
    val similar =
      Spelling
        .selectClosestMatches(command, subcommands.toList.map(_.name))
    val suggestions =
      if (similar.isEmpty)
        "\n\nAvailable subcommands are\n" +
        subcommands.toList
          .map(CLIOutput.indent + _.name + "\n")
          .mkString
      else
        "\n\nThe most similar subcommands are\n" +
        similar.map(CLIOutput.indent + _ + "\n").mkString
    addError(s"`$command` is not a valid subcommand." + suggestions)
    commandProvidedButInvalid = true
    ParserContinuation.Stop
  }

  override private[cli] def result(commandPrefix: Seq[String]) = {
    val prefix = extendPrefix(commandPrefix)
    selectedCommand match {
      case Some(command) =>
        command.opts.result(prefix).addErrors(errors.reverse)
      case None =>
        if (commandProvidedButInvalid)
          Left(OptsParseError(NonEmptyList.fromListUnsafe(errors.reverse)))
        else
          Left(OptsParseError.requestingFullHelp("Expected a subcommand."))
            .addErrors(errors.reverse)
    }
  }
}
