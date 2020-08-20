package org.enso.cli.internal

class AdditionalArguments(helpComment: String) extends BaseOpts[Seq[String]] {
  var value: Seq[String]                        = Seq()
  override private[cli] val additionalArguments = Some(args => value = args)

  override private[cli] def result(commandPrefix: Seq[String]) = Right(value)
  override private[cli] def reset(): Unit =
    value = Seq()

  override def availableOptionsHelp(): Seq[String] =
    if (helpComment.nonEmpty) {
      Seq(helpComment)
    } else Seq()
}
