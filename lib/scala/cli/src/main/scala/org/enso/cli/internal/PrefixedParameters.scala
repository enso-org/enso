package org.enso.cli.internal

class PrefixedParameters(prefix: String, helpComment: Option[String])
    extends BaseOpts[Seq[(String, String)]] {
  if (prefix.exists(_.isWhitespace)) {
    throw new IllegalArgumentException(
      s"Prefix '$prefix' cannot contain whitespace."
    )
  }

  override private[cli] val prefixedParameters = Map(prefix -> update)
  var currentValue: List[(String, String)]     = Nil
  override private[cli] def reset(): Unit      = currentValue = Nil

  private def update(name: String, value: String): Unit = {
    currentValue = (name, value) :: currentValue
  }

  override private[cli] def result() = Right(currentValue.reverse)

  override def additionalHelp(): Seq[String] = helpComment.toSeq
}
