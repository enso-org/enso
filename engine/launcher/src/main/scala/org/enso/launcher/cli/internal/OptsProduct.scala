package org.enso.launcher.cli.internal

import org.enso.launcher.cli.{IllegalOptsStructure, Opts}

class OptsProduct[A, B](lhs: Opts[A], rhs: Opts[B]) extends Opts[(A, B)] {
  override private[cli] val flags      = lhs.flags ++ rhs.flags
  override private[cli] val parameters = lhs.parameters ++ rhs.parameters
  override private[cli] val prefixedParameters =
    lhs.prefixedParameters ++ rhs.prefixedParameters
//  override private[cli] val requiredParameters: Seq[String] =
//    lhs.requiredParameters ++ rhs.requiredParameters

  override private[cli] def wantsArgument() =
    lhs.wantsArgument() || rhs.wantsArgument()
  override private[cli] def consumeArgument(arg: String): Unit =
    if (lhs.wantsArgument()) lhs.consumeArgument(arg)
    else rhs.consumeArgument(arg)
  override private[cli] val requiredArguments: Seq[String] =
    lhs.requiredArguments ++ rhs.requiredArguments
  override private[cli] val optionalArguments: Seq[String] =
    lhs.optionalArguments ++ rhs.optionalArguments
  override private[cli] val trailingArguments: Option[String] =
    OptsProduct.combineOptions(
      lhs.trailingArguments,
      rhs.trailingArguments,
      "More than one set of trailing arguments expected."
    )

  override private[cli] val additionalArguments = OptsProduct.combineOptions(
    lhs.additionalArguments,
    rhs.additionalArguments,
    "More than one instance of additional arguments defined"
  )

  override private[cli] def reset(): Unit = {
    lhs.reset()
    rhs.reset()
  }

  override private[cli] def result() =
    for {
      l <- lhs.result()
      r <- rhs.result()
    } yield (l, r)

  override def helpExplanations(): Seq[String] =
    lhs.helpExplanations() ++ rhs.helpExplanations()
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
