package org.enso.launcher.cli.impl

import org.enso.launcher.cli.Opts

abstract class BaseOpts[A] extends Opts[A] {
  override private[cli] val flags: Map[String, () => Unit]          = Map.empty
  override private[cli] val parameters: Map[String, String => Unit] = Map.empty
  override private[cli] val prefixedParameters
    : Map[String, (String, String) => Unit] = Map.empty
//  override private[cli] val requiredParameters: Seq[String] = Seq()

  override private[cli] def wantsArgument() = false
  override private[cli] def consumeArgument(arg: String): Unit =
    throw new IllegalStateException(
      "Internal error: " +
      "Argument provided even though it was marked as not expected."
    )
  override private[cli] val requiredArguments: Seq[String]    = Seq()
  override private[cli] val optionalArguments: Seq[String]    = Seq()
  override private[cli] val trailingArguments: Option[String] = None

  override private[cli] val additionalArguments: Option[Seq[String] => Unit] =
    None

  override private[cli] def reset(): Unit = {}
}
