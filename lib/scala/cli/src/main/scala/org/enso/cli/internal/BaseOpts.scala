package org.enso.cli.internal

import org.enso.cli.Opts

abstract class BaseOpts[A] extends Opts[A] {
  override private[cli] val flags: Map[String, () => Unit]          = Map.empty
  override private[cli] val parameters: Map[String, String => Unit] = Map.empty
  override private[cli] val prefixedParameters
    : Map[String, (String, String) => Unit]                      = Map.empty
  override private[cli] val usageOptions: Seq[String]            = Seq()
  override private[cli] def gatherOptions: Seq[(String, String)] = Seq()
  override private[cli] def gatherPrefixedParameters: Seq[(String, String)] =
    Seq()

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

  override def availableOptionsHelp(): Seq[String]            = Seq()
  override def availablePrefixedParametersHelp(): Seq[String] = Seq()
  override def additionalHelp(): Seq[String]                  = Seq()
}
