package org.enso.launcher.cli.impl

import org.enso.launcher.cli.Opts

class OptsMap[A, B](a: Opts[A], f: A => B) extends Opts[B] {
  override private[cli] val flags              = a.flags
  override private[cli] val parameters         = a.parameters
  override private[cli] val prefixedParameters = a.prefixedParameters
//  override private[cli] val requiredParameters: Seq[String] =
//    a.requiredParameters

  override private[cli] def wantsArgument() = a.wantsArgument()
  override private[cli] def consumeArgument(arg: String): Unit =
    a.consumeArgument(arg)
  override private[cli] val requiredArguments: Seq[String] = a.requiredArguments
  override private[cli] val optionalArguments: Seq[String] = a.optionalArguments
  override private[cli] val trailingArguments: Option[String] =
    a.trailingArguments

  override private[cli] val additionalArguments = a.additionalArguments

  override private[cli] def reset(): Unit = a.reset()

  override private[cli] def result() = a.result().map(f)

  override def helpExplanations(): Seq[String] = a.helpExplanations()

}
