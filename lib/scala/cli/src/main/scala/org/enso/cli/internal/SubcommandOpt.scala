package org.enso.cli.internal

import cats.data.NonEmptyList
import org.enso.cli.{CLIOutput, Command, OptsParseError, Spelling}

class SubcommandOpt[A](subcommands: NonEmptyList[Command[A]])
    extends BaseSubcommandOpt[A, A] {
  override def availableSubcommands: NonEmptyList[Command[A]] = subcommands

  var commandProvidedButInvalid: Boolean = false

  override def handleUnknownCommand(
    command: String,
    commandPrefix: Seq[String]
  ): ParserContinuation = {
    val similar =
      Spelling
        .selectClosestMatches(command, subcommands.toList.map(_.name))
    val suggestions =
      if (similar.isEmpty)
        "\n\nPossible subcommands are\n" +
        subcommands.toList
          .map(CLIOutput.indent + _.name + "\n")
          .mkString
      else
        "\n\nThe most similar subcommands are\n" +
        similar.map(CLIOutput.indent + _ + "\n").mkString
    addError(s"`$command` is not a valid subcommand." + suggestions)
    commandProvidedButInvalid = true
    ParserContinuation.ContinueNormally
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
          Left(OptsParseError("Expected a subcommand.", help(prefix)))
            .addErrors(errors.reverse)
    }
  }
}
