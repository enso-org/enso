package org.enso.launcher.cli.internal

import org.enso.launcher.cli.Opts

class OptsMap[A, B](a: Opts[A], f: A => B) extends Opts[B] {
  override private[cli] def flags              = a.flags
  override private[cli] def parameters         = a.parameters
  override private[cli] def prefixedParameters = a.prefixedParameters
  override private[cli] def usageOptions       = a.usageOptions
//  override private[cli] val requiredParameters: Seq[String] =
//    a.requiredParameters

  override private[cli] def wantsArgument() = a.wantsArgument()
  override private[cli] def consumeArgument(arg: String): Unit =
    a.consumeArgument(arg)
  override private[cli] def requiredArguments: Seq[String] = a.requiredArguments
  override private[cli] def optionalArguments: Seq[String] = a.optionalArguments
  override private[cli] def trailingArguments: Option[String] =
    a.trailingArguments

  override private[cli] def additionalArguments = a.additionalArguments

  override private[cli] def reset(): Unit = a.reset()

  override private[cli] def result() = a.result().map(f)

  override def helpExplanations(): Seq[String] = a.helpExplanations()
  override def additionalHelp(): Seq[String]   = a.additionalHelp()
}
