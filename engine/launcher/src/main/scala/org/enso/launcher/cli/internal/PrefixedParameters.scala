package org.enso.launcher.cli.internal

class PrefixedParameters(prefix: String, helpComment: String)
    extends BaseOpts[Seq[(String, String)]] {
  override private[cli] val prefixedParameters = Map(prefix -> update)
  var currentValue: List[(String, String)]     = Nil

  private def update(name: String, value: String): Unit = {
    currentValue = (name, value) :: currentValue
  }

  override private[cli] def result() = Right(currentValue.reverse)

  override def helpExplanations(): Seq[String] =
    if (helpComment.nonEmpty) Seq(helpComment) else Seq()
}
