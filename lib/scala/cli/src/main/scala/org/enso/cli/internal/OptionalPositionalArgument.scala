package org.enso.cli.internal

import org.enso.cli.Argument

class OptionalPositionalArgument[A: Argument](
  metavar: String,
  helpComment: Option[String]
) extends BaseOpts[Option[A]] {
  val empty                                  = Right(None)
  var value: Either[List[String], Option[A]] = empty

  override private[cli] val optionalArguments = Seq(metavar)

  override private[cli] def wantsArgument() =
    value match {
      case Right(None) => true
      case _           => false
    }

  override private[cli] def consumeArgument(arg: String): Unit = {
    value = for {
      parsed <- Argument[A].read(arg)
    } yield Some(parsed)
  }

  override private[cli] def reset(): Unit = {
    value = empty
  }

  override private[cli] def result(commandPrefix: Seq[String]) =
    value

  override def additionalHelp(): Seq[String] = helpComment.toSeq
}
