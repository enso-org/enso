package org.enso.cli.internal

import org.enso.cli.Opts

class OptsMap[A, B](a: Opts[A], f: A => B) extends Opts[B] {
  override private[cli] def flags              = a.flags
  override private[cli] def parameters         = a.parameters
  override private[cli] def prefixedParameters = a.prefixedParameters
  override private[cli] def usageOptions       = a.usageOptions
  override private[cli] def gatherOptions =
    a.gatherOptions
  override private[cli] def gatherPrefixedParameters =
    a.gatherPrefixedParameters

  override private[cli] def wantsArgument() = a.wantsArgument()
  override private[cli] def consumeArgument(arg: String): Unit =
    a.consumeArgument(arg)
  override private[cli] def requiredArguments: Seq[String] = a.requiredArguments
  override private[cli] def optionalArguments: Seq[String] = a.optionalArguments
  override private[cli] def trailingArguments: Option[String] =
    a.trailingArguments

  override private[cli] def additionalArguments = a.additionalArguments

  override private[cli] def reset(): Unit = a.reset()

  override private[cli] def result(
    commandPrefix: Seq[String]
  ): Either[List[String], B] = a.result(commandPrefix).map(f)

  override def availableOptionsHelp(): Seq[String] = a.availableOptionsHelp()
  override def availablePrefixedParametersHelp(): Seq[String] =
    a.availablePrefixedParametersHelp()
  override def additionalHelp(): Seq[String] = a.additionalHelp()
}
