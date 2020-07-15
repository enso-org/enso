package org.enso.launcher.cli.internal

import org.enso.launcher.cli.Argument

class OptionalParameter[A: Argument](
  name: String,
  metavar: String,
  helpComment: String
) extends BaseOpts[Option[A]] {
  override private[cli] val parameters =
    Map(name -> update)

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

  override def helpExplanations(): Seq[String] = {
    Seq(s"--$name <$metavar>\t$helpComment")
  }
}
