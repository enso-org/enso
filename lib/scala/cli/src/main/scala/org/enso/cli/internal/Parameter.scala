package org.enso.cli.internal

import org.enso.cli.Argument

class Parameter[A: Argument](
  name: String,
  metavar: String,
  helpComment: String
) extends BaseOpts[A] {
  if (name.exists(_.isWhitespace)) {
    throw new IllegalArgumentException(
      s"Parameter name '$name' cannot contain whitespace."
    )
  }

  override private[cli] val parameters =
    Map(name -> update)
  override private[cli] val usageOptions =
    Seq(s"--$name $metavar")
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

  override private[cli] def result() =
    value.flatMap {
      case Some(value) => Right(value)
      case None        => Left(List(s"Missing required parameter $name"))
    }

  override def availableOptionsHelp(): Seq[String] = {
    Seq(s" --$name $metavar\t$helpComment")
  }
}
