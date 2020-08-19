package org.enso.cli.internal

import org.enso.cli.{IllegalOptsStructure, Opts}

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
  override private[cli] def consumeArgument(arg: String): Unit =
    if (lhs.wantsArgument()) lhs.consumeArgument(arg)
    else rhs.consumeArgument(arg)
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
  ): Either[List[String], (A, B)] =
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
