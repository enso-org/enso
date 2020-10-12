package org.enso.cli.internal.opts

import org.enso.cli.arguments.{Argument, OptsParseError}
import org.enso.cli.internal.ParserContinuation

import scala.annotation.unused

class TrailingArguments[A: Argument](
  metavar: String,
  helpComment: Option[String]
) extends BaseOpts[Seq[A]] {
  val empty                                  = Right(Nil)
  var value: Either[OptsParseError, List[A]] = empty

  override private[cli] val trailingArguments = Some(metavar)

  override private[cli] def wantsArgument() = true

  override private[cli] def consumeArgument(
    arg: String,
    @unused commandPrefix: Seq[String],
    @unused suppressUnexpectedArgument: Boolean
  ): ParserContinuation = {
    value = for {
      currentArguments <- value
      parsed           <- Argument[A].read(arg)
    } yield parsed :: currentArguments
    ParserContinuation.ContinueNormally
  }

  override private[cli] def reset(): Unit = {
    value = empty
  }

  override private[cli] def result(commandPrefix: Seq[String]) =
    value.map(_.reverse)

  override def additionalHelp(): Seq[String] = helpComment.toSeq
}
