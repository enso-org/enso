package org.enso.cli.internal.opts

import org.enso.cli.arguments.OptsParseError

class Flag(
  name: String,
  short: Option[Char],
  helpComment: String,
  showInUsage: Boolean
) extends BaseOpts[Boolean] {
  if (name.exists(_.isWhitespace)) {
    throw new IllegalArgumentException(
      s"Flag name '$name' cannot contain whitespace."
    )
  }

  override private[cli] val flags = {
    val shortParser = for {
      opt <- short
    } yield opt.toString -> (update _)

    Map(name -> (update _)) ++ shortParser
  }
  override private[cli] val usageOptions =
    if (showInUsage) Seq(s"[--$name]") else Seq()
  override private[cli] def gatherOptions =
    Seq(name -> s"--$name") ++
    short.map(char => char.toString -> s"-$char").toSeq

  val empty                                  = Right(false)
  var value: Either[OptsParseError, Boolean] = empty

  override private[cli] def reset(): Unit = {
    value = empty
  }

  private def update(): Unit = {
    value = value.flatMap(
      if (_) OptsParseError.left(s"Flag $name is set more than once")
      else Right(true)
    )
  }

  override private[cli] def result(commandPrefix: Seq[String]) = value

  override def availableOptionsHelp(): Seq[String] =
    short match {
      case Some(short) =>
        Seq(s"[-$short | --$name]\t$helpComment")
      case None =>
        Seq(s"[--$name]\t$helpComment")
    }
}
