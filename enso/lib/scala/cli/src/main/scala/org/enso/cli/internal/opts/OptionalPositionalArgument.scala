package org.enso.cli.internal.opts

import org.enso.cli.arguments.{Argument, OptsParseError}
import org.enso.cli.internal.ParserContinuation

import scala.annotation.unused

class OptionalPositionalArgument[A: Argument](
  metavar: String,
  helpComment: Option[String]
) extends BaseOpts[Option[A]] {
  val empty                                    = Right(None)
  var value: Either[OptsParseError, Option[A]] = empty

  override private[cli] val optionalArguments = Seq(metavar)

  override private[cli] def wantsArgument() =
    value match {
      case Right(None) => true
      case _           => false
    }

  override private[cli] def consumeArgument(
    arg: String,
    @unused commandPrefix: Seq[String],
    @unused suppressUnexpectedArgument: Boolean
  ): ParserContinuation = {
    value = for {
      parsed <- Argument[A].read(arg)
    } yield Some(parsed)
    ParserContinuation.ContinueNormally
  }

  override private[cli] def reset(): Unit = {
    value = empty
  }

  override private[cli] def result(commandPrefix: Seq[String]) =
    value

  override def additionalHelp(): Seq[String] = helpComment.toSeq
}
