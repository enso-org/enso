package org.enso.launcher.cli.internal

import org.enso.launcher.cli.Argument

class TrailingArguments[A: Argument](
  metavar: String,
  helpComment: String
) extends BaseOpts[Seq[A]] {
  val empty                                = Right(Nil)
  var value: Either[List[String], List[A]] = empty

  override private[cli] val requiredArguments = Seq(metavar)

  override private[cli] def wantsArgument() = true

  override private[cli] def consumeArgument(arg: String): Unit = {
    value = for {
      currentArguments <- value
      parsed           <- Argument[A].read(arg)
    } yield parsed :: currentArguments
  }

  override private[cli] def reset(): Unit = {
    value = empty
  }

  override private[cli] def result() =
    value.map(_.reverse)

  override def helpExplanations(): Seq[String] = {
    if (helpComment.nonEmpty) {
      Seq(s"<$metavar>...\t$helpComment")
    } else Seq()
  }
}
