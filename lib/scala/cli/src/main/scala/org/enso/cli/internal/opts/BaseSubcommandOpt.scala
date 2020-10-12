package org.enso.cli.internal.opts

import cats.data.NonEmptyList
import org.enso.cli.arguments.{Command, Opts}
import org.enso.cli.internal.ParserContinuation

/**
  * Implements common logic for options that can take a command and modify their
  * further parsing behavior based on that command.
  * @tparam A returned type
  * @tparam B type returned by the commands
  */
trait BaseSubcommandOpt[A, B] extends Opts[A] {

  /**
    * Lists all available commands.
    */
  def availableSubcommands: NonEmptyList[Command[B]]

  /**
    * Handles an unknown command.
    *
    * Executed when a command is given that does not match any of
    * [[availableSubcommands]].
    */
  def handleUnknownCommand(command: String): ParserContinuation

  /**
    * The command that has been selected, if any.
    *
    * If no command was selected, is set to None.
    */
  var selectedCommand: Option[Command[B]] = None

  /**
    * List of errors reported when parsing this set of options.
    */
  var errors: List[String] = Nil

  /**
    * Adds an error that will be reported when getting the result.
    */
  def addError(error: String): Unit = errors ::= error

  /**
    * Extends a command prefix from the call with the currently selected command
    * (if a command is selected).
    */
  def extendPrefix(commandPrefix: Seq[String]): Seq[String] =
    commandPrefix ++ selectedCommand.map(_.name)

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
  override private[cli] def usageOptions =
    availableSubcommands.toList.flatMap(_.opts.usageOptions).distinct

  override private[cli] def wantsArgument() =
    selectedCommand.map(_.opts.wantsArgument()).getOrElse(true)

  override private[cli] def consumeArgument(
    arg: String,
    commandPrefix: Seq[String],
    suppressUnexpectedArgument: Boolean
  ): ParserContinuation = {
    val prefix = extendPrefix(commandPrefix)
    selectedCommand match {
      case Some(command) =>
        command.opts.consumeArgument(arg, prefix, suppressUnexpectedArgument)
      case None =>
        availableSubcommands.find(_.name == arg) match {
          case Some(command) =>
            selectedCommand = Some(command)
            ParserContinuation.ContinueNormally
          case None =>
            Command.formatRelated(
              arg,
              prefix,
              availableSubcommands.toList
            ) match {
              case Some(relatedMessage) =>
                addError(relatedMessage)
                ParserContinuation.Stop
              case None =>
                if (suppressUnexpectedArgument) ParserContinuation.Stop
                else handleUnknownCommand(arg)
            }
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

  /**
    * @inheritdoc
    */
  override def availableOptionsHelp(): Seq[String] =
    availableSubcommands.toList.flatMap(_.opts.availableOptionsHelp()).distinct

  /**
    * @inheritdoc
    */
  override def availablePrefixedParametersHelp(): Seq[String] =
    availableSubcommands.toList
      .flatMap(_.opts.availablePrefixedParametersHelp())
      .distinct

  /**
    * @inheritdoc
    */
  override def additionalHelp(): Seq[String] =
    availableSubcommands.toList.flatMap(_.opts.additionalHelp()).distinct

  /**
    * @inheritdoc
    */
  override def commandLines(
    alwaysIncludeOtherOptions: Boolean = false
  ): NonEmptyList[String] = {
    def prefixedCommandLines(command: Command[_]): NonEmptyList[String] = {
      val prefix = command.name + " "
      val suffix = s"\n\t${command.comment}"
      command.opts
        .commandLines(alwaysIncludeOtherOptions)
        .map(commandLine => prefix + commandLine + suffix)
    }

    availableSubcommands.flatMap(prefixedCommandLines)
  }

  /**
    * @inheritdoc
    */
  override def shortHelp(commandPrefix: Seq[String]): String =
    super.shortHelp(extendPrefix(commandPrefix))
}
