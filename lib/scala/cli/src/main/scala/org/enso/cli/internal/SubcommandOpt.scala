package org.enso.cli.internal

import cats.data.NonEmptyList
import org.enso.cli.{CLIOutput, Opts, Spelling, Subcommand}

class SubcommandOpt[A](subcommands: NonEmptyList[Subcommand[A]])
    extends Opts[A] {
  var selectedCommand: Option[Subcommand[A]] = None
  var errors: List[String]                   = Nil
  def addError(error: String): Unit          = errors ::= error

  override private[cli] def flags =
    selectedCommand.map(_.opts.flags).getOrElse(Map.empty)
  override private[cli] def parameters =
    selectedCommand.map(_.opts.parameters).getOrElse(Map.empty)
  override private[cli] def prefixedParameters =
    selectedCommand.map(_.opts.prefixedParameters).getOrElse(Map.empty)
  override private[cli] def gatherOptions =
    selectedCommand.map(_.opts.gatherOptions).getOrElse(Seq())
  override private[cli] def gatherPrefixedParameters =
    selectedCommand.map(_.opts.gatherPrefixedParameters).getOrElse(Seq())
  override private[cli] val usageOptions =
    subcommands.toList.flatMap(_.opts.usageOptions).distinct

  override private[cli] def wantsArgument() =
    selectedCommand.map(_.opts.wantsArgument()).getOrElse(true)

  override private[cli] def consumeArgument(arg: String): Unit =
    selectedCommand match {
      case Some(command) => command.opts.consumeArgument(arg)
      case None =>
        subcommands.find(_.name == arg) match {
          case Some(command) =>
            selectedCommand = Some(command)
          case None =>
            val similar =
              Spelling
                .selectClosestMatches(arg, subcommands.toList.map(_.name))
            val suggestions =
              if (similar.isEmpty)
                "\n\nPossible subcommands are\n" +
                subcommands.toList
                  .map(CLIOutput.indent + _.name + "\n")
                  .mkString
              else
                "\n\nThe most similar subcommands are\n" +
                similar.map(CLIOutput.indent + _ + "\n").mkString
            addError(s"`$arg` is not a valid subcommand." + suggestions)

        }
    }

  override private[cli] def requiredArguments =
    selectedCommand.map(_.opts.requiredArguments).getOrElse(Seq())
  override private[cli] def optionalArguments =
    selectedCommand.map(_.opts.optionalArguments).getOrElse(Seq())
  override private[cli] def trailingArguments =
    selectedCommand.flatMap(_.opts.trailingArguments)
  override private[cli] def additionalArguments =
    selectedCommand.flatMap(_.opts.additionalArguments)

  override private[cli] def reset(): Unit = {
    subcommands.map(_.opts.reset())
    selectedCommand = None
    errors          = Nil
  }

  override private[cli] def result(commandPrefix: Seq[String]) =
    if (errors.nonEmpty)
      Left(errors.reverse)
    else
      selectedCommand match {
        case Some(command) => command.opts.result(commandPrefix)
        case None =>
          Left(List("Expected a subcommand.", help(commandPrefix)))
      }

  override def availableOptionsHelp(): Seq[String] =
    subcommands.toList.flatMap(_.opts.availableOptionsHelp()).distinct

  override def availablePrefixedParametersHelp(): Seq[String] =
    subcommands.toList
      .flatMap(_.opts.availablePrefixedParametersHelp())
      .distinct

  override def additionalHelp(): Seq[String] =
    subcommands.toList.flatMap(_.opts.additionalHelp()).distinct

  override def commandLines(): NonEmptyList[String] = {
    def prefixedCommandLines(command: Subcommand[_]): NonEmptyList[String] = {
      val prefix = command.name + " "
      val suffix = s"\n\t${command.comment}"
      command.opts
        .commandLines()
        .map(commandLine => prefix + commandLine + suffix)
    }

    subcommands.flatMap(prefixedCommandLines)
  }
}
