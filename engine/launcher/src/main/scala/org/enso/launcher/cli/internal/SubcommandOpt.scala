package org.enso.launcher.cli.internal

import cats.data.NonEmptyList
import org.enso.launcher.cli.{Command, Opts}

class SubcommandOpt[A](subcommands: NonEmptyList[Command[A]]) extends Opts[A] {
  var selectedCommand: Option[Command[A]] = None
  var errors: List[String]                = Nil
  def addError(error: String): Unit       = errors ::= error

  override private[cli] def flags =
    selectedCommand.map(_.opts.flags).getOrElse(Map.empty)
  override private[cli] def parameters =
    selectedCommand.map(_.opts.parameters).getOrElse(Map.empty)
  override private[cli] def prefixedParameters =
    selectedCommand.map(_.opts.prefixedParameters).getOrElse(Map.empty)
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
            addError(s"Subcommand '$arg' is not valid.")
          // TODO [RW] Suggestions
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

  override private[cli] def result() =
    if (errors.nonEmpty)
      Left(errors.reverse)
    else
      selectedCommand match {
        case Some(command) => command.opts.result()
        case None =>
          Left(List("Expected a subcommand.")) // TODO [RW] suggestions?
      }

  override def helpExplanations(): Seq[String] =
    subcommands.toList.flatMap(_.opts.helpExplanations()).distinct

  override def additionalHelp(): Seq[String] =
    subcommands.toList.flatMap(_.opts.additionalHelp()).distinct

  override def commandLines(): NonEmptyList[String] = {
    def prefixedCommandLines(command: Command[_]): NonEmptyList[String] =
      command.opts.commandLines().map(command.name + " " + _)

    subcommands.flatMap(prefixedCommandLines)
  }
}
