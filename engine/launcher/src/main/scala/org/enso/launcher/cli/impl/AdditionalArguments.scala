package org.enso.launcher.cli.impl

class AdditionalArguments(helpComment: String) extends BaseOpts[Seq[String]] {
  var value: Seq[String]                        = Seq()
  override private[cli] val additionalArguments = Some(args => value = args)

  override private[cli] def result() = Right(value)

  override def helpExplanations(): Seq[String] =
    if (helpComment.nonEmpty) {
      Seq(helpComment)
    } else Seq()
}
