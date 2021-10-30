package org.enso.cli.internal.opts

import cats.data.NonEmptyList
import org.enso.cli.arguments.{IllegalOptsStructure, Opts, OptsParseError}
import org.enso.cli.internal.ParserContinuation

class OptsProduct[A, B](lhs: Opts[A], rhs: Opts[B]) extends Opts[(A, B)] {
  override private[cli] def flags      = lhs.flags ++ rhs.flags
  override private[cli] def parameters = lhs.parameters ++ rhs.parameters
  override private[cli] def prefixedParameters =
    lhs.prefixedParameters ++ rhs.prefixedParameters
  override private[cli] def usageOptions =
    lhs.usageOptions ++ rhs.usageOptions
  override private[cli] def gatherOptions =
    lhs.gatherOptions ++ rhs.gatherOptions
  override private[cli] def gatherPrefixedParameters =
    lhs.gatherPrefixedParameters ++ rhs.gatherPrefixedParameters

  override private[cli] def wantsArgument() =
    lhs.wantsArgument() || rhs.wantsArgument()
  override private[cli] def consumeArgument(
    arg: String,
    commandPrefix: Seq[String],
    suppressUnexpectedArgument: Boolean
  ): ParserContinuation =
    if (lhs.wantsArgument())
      lhs.consumeArgument(
        arg,
        commandPrefix,
        suppressUnexpectedArgument
      )
    else rhs.consumeArgument(arg, commandPrefix, suppressUnexpectedArgument)
  override private[cli] def requiredArguments: Seq[String] =
    lhs.requiredArguments ++ rhs.requiredArguments
  override private[cli] def optionalArguments: Seq[String] =
    lhs.optionalArguments ++ rhs.optionalArguments
  override private[cli] def trailingArguments: Option[String] =
    OptsProduct.combineOptions(
      lhs.trailingArguments,
      rhs.trailingArguments,
      "More than one set of trailing arguments expected."
    )

  override private[cli] def additionalArguments =
    OptsProduct.combineOptions(
      lhs.additionalArguments,
      rhs.additionalArguments,
      "More than one instance of additional arguments defined"
    )

  override private[cli] def reset(): Unit = {
    lhs.reset()
    rhs.reset()
  }

  override private[cli] def result(
    commandPrefix: Seq[String]
  ): Either[OptsParseError, (A, B)] =
    for {
      l <- lhs.result(commandPrefix)
      r <- rhs.result(commandPrefix)
    } yield (l, r)

  override def availableOptionsHelp(): Seq[String] =
    lhs.availableOptionsHelp() ++ rhs.availableOptionsHelp()
  override def availablePrefixedParametersHelp(): Seq[String] =
    lhs.availablePrefixedParametersHelp() ++
    rhs.availablePrefixedParametersHelp()
  override def additionalHelp(): Seq[String] =
    lhs.additionalHelp() ++ rhs.additionalHelp()

  /** A helper function that gathers all options and arguments definitions, to
    * display a command line for showing in the usage section of the help.
    *
    * This variant is special to ensure proper handling of subcommands.
    *
    * Subcommands can be theoretically nested, but they cannot be grouped next
    * to each other (because it would cause ordering problems and would be
    * unintuitive for users). So in a product, at most one of the pair can be a
    * subcommand. We compute command lines for both elements and check if one of
    * them has more than one command line - that would indicate a subcommand. If
    * we find a subcommand, we use its command lines. Otherwise, we just call
    * the  original implementation.
    */
  override def commandLines(
    alwaysIncludeOtherOptions: Boolean = false
  ): NonEmptyList[String] = {
    val rightHasOptions = rhs.gatherOptions.nonEmpty
    val leftHasOptions  = lhs.gatherOptions.nonEmpty

    val left = lhs.commandLines(rightHasOptions || alwaysIncludeOtherOptions)
    if (left.size > 1) left
    else {
      val right = rhs.commandLines(leftHasOptions || alwaysIncludeOtherOptions)
      if (right.size > 1) right
      else super.commandLines(alwaysIncludeOtherOptions)
    }
  }
}

object OptsProduct {
  private def combineOptions[A](
    lhs: Option[A],
    rhs: Option[A],
    duplicateMessage: String
  ): Option[A] =
    (lhs, rhs) match {
      case (None, None)       => None
      case (Some(l), None)    => Some(l)
      case (None, Some(r))    => Some(r)
      case (Some(_), Some(_)) => throw IllegalOptsStructure(duplicateMessage)
    }
}
