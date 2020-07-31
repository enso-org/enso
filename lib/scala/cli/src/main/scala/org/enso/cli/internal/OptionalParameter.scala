package org.enso.cli.internal

import org.enso.cli.Argument

class OptionalParameter[A: Argument](
  name: String,
  metavar: String,
  helpComment: String,
  showInUsage: Boolean
) extends BaseOpts[Option[A]] {
  if (name.exists(_.isWhitespace)) {
    throw new IllegalArgumentException(
      s"Optional parameter name '$name' cannot contain whitespace."
    )
  }

  override private[cli] val parameters =
    Map(name -> update)
  override private[cli] val usageOptions =
    if (showInUsage) Seq(s"[--$name $metavar]") else Seq()
  override private[cli] def gatherOptions =
    Seq(name -> s"--$name $metavar")

  val empty = Right(None)

  var value: Either[List[String], Option[A]] = empty

  override private[cli] def reset(): Unit = {
    value = empty
  }

  private def update(newValue: String): Unit = {
    value = combineWithoutDuplicates(
      value,
      Argument[A].read(newValue),
      s"Multiple values for parameter $name."
    )
  }

  override private[cli] def result() = value

  override def availableOptionsHelp(): Seq[String] = {
    Seq(s"[--$name $metavar]\t$helpComment")
  }
}
