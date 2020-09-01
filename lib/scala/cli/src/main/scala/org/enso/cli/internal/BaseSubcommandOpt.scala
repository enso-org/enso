package org.enso.cli.internal

import cats.data.NonEmptyList
import org.enso.cli.{Command, Opts}

trait BaseSubcommandOpt[A, B] extends Opts[A] {
  def availableSubcommands:                  NonEmptyList[Command[B]]
  def handleUnknownCommand(command: String): ParserContinuation

  var selectedCommand: Option[Command[B]] = None
  var errors: List[String]                = Nil
  def addError(error: String): Unit       = errors ::= error

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
    availableSubcommands.toList.flatMap(_.opts.usageOptions).distinct

  override private[cli] def wantsArgument() =
    selectedCommand.map(_.opts.wantsArgument()).getOrElse(true)

  override private[cli] def consumeArgument(
    arg: String,
    commandPrefix: Seq[String]
  ): ParserContinuation =
    selectedCommand match {
      case Some(command) => command.opts.consumeArgument(arg, commandPrefix)
      case None =>
        availableSubcommands.find(_.name == arg) match {
          case Some(command) =>
            selectedCommand = Some(command)
            ParserContinuation.ContinueNormally
          case None =>
            Command.formatRelated(
              arg,
              commandPrefix,
              availableSubcommands.toList
            ) match {
              case Some(relatedMessage) =>
                addError(relatedMessage)
                ParserContinuation.ContinueNormally
              case None =>
                handleUnknownCommand(arg)
            }

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
    availableSubcommands.map(_.opts.reset())
    selectedCommand = None
    errors          = Nil
  }
  override def availableOptionsHelp(): Seq[String] =
    availableSubcommands.toList.flatMap(_.opts.availableOptionsHelp()).distinct

  override def availablePrefixedParametersHelp(): Seq[String] =
    availableSubcommands.toList
      .flatMap(_.opts.availablePrefixedParametersHelp())
      .distinct

  override def additionalHelp(): Seq[String] =
    availableSubcommands.toList.flatMap(_.opts.additionalHelp()).distinct

  override def commandLines(): NonEmptyList[String] = {
    def prefixedCommandLines(command: Command[_]): NonEmptyList[String] = {
      val prefix = command.name + " "
      val suffix = s"\n\t${command.comment}"
      command.opts
        .commandLines()
        .map(commandLine => prefix + commandLine + suffix)
    }

    availableSubcommands.flatMap(prefixedCommandLines)
  }
}
